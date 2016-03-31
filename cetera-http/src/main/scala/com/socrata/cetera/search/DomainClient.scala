package com.socrata.cetera.search

import com.rojoma.json.v3.util.JsonUtil
import org.elasticsearch.action.search.{SearchRequestBuilder, SearchType}
import org.elasticsearch.index.query.QueryBuilders
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.authentication.CoreClient
import com.socrata.cetera.search.DomainAggregations._
import com.socrata.cetera.search.DomainFilters._
import com.socrata.cetera.types.{Domain, DomainCnameFieldType, QueryType}
import com.socrata.cetera.util.{JsonDecodeException, LogHelper}

class DomainClient(val esClient: ElasticSearchClient, val coreClient: CoreClient, val indexAliasName: String) {
  val logger = LoggerFactory.getLogger(getClass)

  def fetch(id: Int): Option[Domain] = {
    val get = esClient.client.prepareGet(indexAliasName, esDomainType, id.toString)
    logger.info("Elasticsearch request: " + get.request.toString)

    val res = get.execute.actionGet
    Domain(res.getSourceAsString)
  }

  private def find(cnames: Set[String]): (Set[Domain], Long) = {
    val query = QueryBuilders.termsQuery(DomainCnameFieldType.rawFieldName, cnames.toList: _*)

    val search = esClient.client.prepareSearch(indexAliasName).setTypes(esDomainType)
      .setQuery(query)
      .setSize(cnames.size)

    logger.info(LogHelper.formatEsRequest(search))

    val res = search.execute.actionGet
    val timing = res.getTookInMillis
    val domains = res.getHits.hits.flatMap { h =>
      JsonUtil.parseJson[Domain](h.sourceAsString) match {
        case Right(domain) => Some(domain)
        case Left(err) =>
          logger.error(err.english)
          throw new JsonDecodeException(err)
      }
    }.toSet

    (domains, timing)
  }

  private def filterByLockdownStatus(searchContextCname: Option[String],
                             domains: Set[Domain],
                             cookie: Option[String]): Set[Domain] = {
    val lockedDownDomains = domains.filter(d => d.lockedDown || d.apiLockedDown)
    if (lockedDownDomains.isEmpty) {
      domains
    } else {
      val loggedInOnTarget = searchContextCname.exists(coreClient.isCookieValid(_, cookie))
      if (loggedInOnTarget) domains else domains -- lockedDownDomains
    }
  }

  // if domain cname filter is provided limit to that scope, otherwise default to publicly visible domains
  def findRelevantDomains(searchContextCname: Option[String],
                          domainCnames: Set[String],
                          cookie: Option[String]): (Set[Domain], Long) = {
    val (domains, timing) = searchContextCname.foldLeft(domainCnames) { (b, x) => b + x } match {
      case cs: Set[String] if cs.nonEmpty => find(cs)
      case _ => customerDomainSearch
    }
    (filterByLockdownStatus(searchContextCname, domains, cookie), timing)
  }

  // TODO: handle unlimited domain count with aggregation or scan+scroll query
  // if we get 42 thousand customer domains before addressing this, most of us will probably be millionaires anyway.
  private val customerDomainSearchSize = 42000

  // when query doesn't define domain filter, we assume all customer domains.
  private def customerDomainSearch: (Set[Domain], Long) = {
    val search = esClient.client.prepareSearch(indexAliasName).setTypes(esDomainType)
      .setQuery(QueryBuilders.filteredQuery(
        QueryBuilders.matchAllQuery(),
        isCustomerDomainFilter)
      )
      .setSize(customerDomainSearchSize)
    logger.info(LogHelper.formatEsRequest(search))

    val res = search.execute.actionGet
    val timing = res.getTookInMillis
    val domains = res.getHits.hits.flatMap { h =>
      Domain(h.getSourceAsString)
    }.toSet
    (domains, timing)
  }

  // (pre-)calculate domain moderation and R+A status
  def calculateIdsAndModRAStatuses(domains: Set[Domain]): (Set[Int], Set[Int], Set[Int], Set[Int]) = {
    val ids = domains.map(_.domainId)
    val mod = domains.collect { case d: Domain if d.moderationEnabled => d.domainId }
    val unmod = domains.collect { case d: Domain if !d.moderationEnabled => d.domainId }
    val raOff = domains.collect { case d: Domain if !d.routingApprovalEnabled => d.domainId }
    (ids, mod, unmod, raOff)
  }

  def buildCountRequest(
      searchQuery: QueryType,
      searchDomains: Set[Domain],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      only: Option[Seq[String]])
    : SearchRequestBuilder = {
    val (domainsFilter, domainsAggregation) = {
      val contextMod = searchContext.exists(_.moderationEnabled)
      val (ids, mod, unmod, raOff) = calculateIdsAndModRAStatuses(searchDomains)
      val dsFilter = if (ids.nonEmpty) domainIds(ids) else isCustomerDomainFilter
      val dsAggregation = domains(contextMod, mod, unmod, raOff)
      (dsFilter, dsAggregation)
    }
    esClient.client.prepareSearch(indexAliasName).setTypes(esDomainType)
      .setQuery(QueryBuilders.filteredQuery(QueryBuilders.matchAllQuery(), domainsFilter))
      .addAggregation(domainsAggregation)
      .setSearchType(SearchType.COUNT)
      .setSize(0) // no docs, aggs only
  }
}

class DomainNotFound(cname: String) extends NoSuchElementException(cname)
