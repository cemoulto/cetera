package com.socrata.cetera.services

import scala.util.control.NonFatal

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.io.JsonReader
import com.socrata.cetera._
import com.socrata.cetera.handlers.VisibilityParamSet
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.response.DocumentSearchResult
import com.socrata.cetera.response.Format.formatDocumentResponse
import com.socrata.cetera.search.{BaseDocumentClient, BaseDomainClient, DomainNotFound}
import com.socrata.cetera.types._
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util._
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses._
import com.socrata.http.server.routing.SimpleResource
import com.socrata.http.server.{HttpRequest, HttpResponse, HttpService}
import org.elasticsearch.search.SearchHits
import org.slf4j.LoggerFactory


class SearchService(elasticSearchClient: BaseDocumentClient,
                    domainClient: BaseDomainClient,
                    balboaClient: BalboaClient) extends SimpleResource {
  lazy val logger = LoggerFactory.getLogger(classOf[SearchService])

  // When we construct this map, we already have the set of all possible domains
  // TODO: Verify that the "should never happen" clause does not actually happen
  private def extractDomainCnames(domainIdCnames: Map[Int, String], hits: SearchHits): Map[Int, String] = {
    val distinctDomainIds = hits.hits.map { h =>
      JsonReader.fromString(h.sourceAsString()).dyn.socrata_id.domain_id.! match {
        case jn: JNumber => jn.toInt
        case _ => throw new scala.NoSuchElementException
      }
    }.toSet // deduplicate domain id

    val unknownDomainIds = distinctDomainIds -- domainIdCnames.keys // don't repeat lookup for known domains
    if (unknownDomainIds.nonEmpty) logger.warn(s"Domains not known at query construction time: $unknownDomainIds.")
    unknownDomainIds.flatMap { i =>
      // This should never happen!!! ;)
      domainClient.fetch(i).map { d => i -> d.domainCname } // lookup domain cname from elasticsearch
    }.toMap ++ domainIdCnames
  }

  def logSearchTerm(domain: Option[Domain], query: QueryType): Unit = {
    domain.foreach(d =>
      query match {
        case NoQuery => // nothing to log to balboa
        case SimpleQuery(q) => balboaClient.logQuery(d.domainId, q)
        case AdvancedQuery(q) => balboaClient.logQuery(d.domainId, q)
      }
    )
  }

  def doSearch(queryParameters: MultiQueryParams, // scalastyle:ignore parameter.number method.length
               cookie: Option[String],
               extendedHost: Option[String],
               requestId: Option[String]
              ): (SearchResults[DocumentSearchResult], InternalTimings, Seq[String]) = {
    val now = Timings.now()

    QueryParametersParser(queryParameters, extendedHost) match {
      case Left(errors) =>
        val msg = errors.map(_.message).mkString(", ")
        throw new IllegalArgumentException(s"Invalid query parameters: $msg")

      case Right(params) =>
        val (domainSet, domainSearchTime, setCookies) =
          domainClient.findSearchableDomains(params.searchParamSet.searchContext, params.searchParamSet.domains,
            true, cookie, requestId)
        val domainIdBoosts = domainSet.domainIdBoosts(params.relevanceParamSet.domainBoosts)

        // TODO: need approval logic somewhere.
        val visibilityParamSet = VisibilityParamSet(Some("approved"), Some("approved"), Some("approved"))
        val req = elasticSearchClient.buildSearchRequest(params.searchParamSet, visibilityParamSet,
          params.relevanceParamSet, params.pagingParamSet, domainSet)

        logger.info(LogHelper.formatEsRequest(req))
        val res = req.execute.actionGet

        val idCnames = domainSet.idCnameMap

        val formattedResults: SearchResults[DocumentSearchResult] = formatDocumentResponse(idCnames,
          params.relevanceParamSet.showScore, res)
        val timings = InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime, res.getTookInMillis))
        logSearchTerm(domainSet.searchContext, params.searchParamSet.searchQuery)

        (formattedResults.copy(timings = Some(timings)), timings, setCookies)
    }
  }

  def search(req: HttpRequest): HttpResponse = {
    logger.debug(LogHelper.formatHttpRequestVerbose(req))

    val cookie = req.header(HeaderCookieKey)
    val extendedHost = req.header(HeaderXSocrataHostKey)
    val requestId = req.header(HeaderXSocrataRequestIdKey)

    try {
      val (formattedResults, timings, setCookies) = doSearch(req.multiQueryParams, cookie, extendedHost, requestId)
      logger.info(LogHelper.formatRequest(req, timings))
      Http.decorate(Json(formattedResults, pretty = true), OK, setCookies)
    } catch {
      case e: IllegalArgumentException =>
        logger.info(e.getMessage)
        BadRequest ~> HeaderAclAllowOriginAll ~> jsonError(e.getMessage)
      case DomainNotFound(e) =>
        val msg = s"Domain not found: $e"
        logger.error(msg)
        NotFound ~> HeaderAclAllowOriginAll ~> jsonError(msg)
      case NonFatal(e) =>
        val msg = "Cetera search service error"
        val esError = ElasticsearchError(e)
        logger.error(s"$msg: $esError")
        InternalServerError ~> HeaderAclAllowOriginAll ~> jsonError("We're sorry. Something went wrong.")
    }
  }

  // $COVERAGE-OFF$ jetty wiring
  object Service extends SimpleResource {
    override def get: HttpService = search
  }
  // $COVERAGE-ON$
}
