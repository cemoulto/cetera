package com.socrata.cetera.services

import scala.util.control.NonFatal

import com.rojoma.json.v3.ast.{JNumber, JString}
import com.rojoma.json.v3.io.JsonReader
import com.socrata.http.server._
import com.socrata.http.server.responses._
import com.socrata.http.server.routing.SimpleResource
import org.elasticsearch.action.search.SearchResponse
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.authentication.CoreClient
import com.socrata.cetera.search.{BaseDocumentClient, DocumentClient, DomainNotFound}
import com.socrata.cetera.types.Domain
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util.{InternalTimings, SearchResults, _}

class ProfileSearchService(elasticSearchClient: BaseDocumentClient, coreClient: CoreClient) {
  lazy val logger = LoggerFactory.getLogger(getClass)

  // TODO possibly consolidate auth into one place (this func template pulled from UserService)
  def verifyUserAuthentication(
                               cookie: Option[String],
                               extendedHost: Option[String],
                               requestId: Option[String])
  : (Boolean, Seq[String]) = {
    (cookie, extendedHost) match {
      case (Some(c), Some(h)) =>
        val (authUser, setCookies) = coreClient.fetchUserByCookie(h, c, requestId)
        (authUser.nonEmpty, setCookies)
      case _ =>
        (false, Seq.empty[String])
    }
  }

//  this should take in a cookie, then verifyUserAuthorization, and, if authorized, build the query to return
//  both Owned and SharedTo (filters already written)
//  do I need a ProfileClient, or something like UserClient?
def doSearch(queryParameters: MultiQueryParams,
             cookie: Option[String],
             extendedHost: Option[String],
             requestId: Option[String]
            ): (SearchResults[SearchResult], InternalTimings, Seq[String]) = {
  val now = Timings.now()

  val (authorized, setCookies) = verifyUserAuthentication(cookie, extendedHost, requestId)

  if (authorized) {
    QueryParametersParser(queryParameters, extendedHost) match {
      case Left(errors) =>
        val msg = errors.map(_.message).mkString(", ")
        throw new IllegalArgumentException(s"Invalid query parameters: $msg")

      case Right(params) =>
        // TODO this is stupid. When we figure out what search request we're using, we'll pass in correct domain
        val domain = Domain(
          1,
          params.domains.getOrElse(Set.empty[String]).head,
          None,
          None,
          false,
          false,
          false,
          false,
          false
        )
        val req = elasticSearchClient.buildSearchRequest(
          params.searchQuery,
          Set(domain), // TODO consider validating that it's only one domain
          params.domainMetadata,
          None, // search context
          params.categories,
          params.tags,
          params.only,
          params.owner, // this shows all datasets owned by this user (which could be me) (maybe params.owner),
          params.sharedTo,
          params.parentDatasetId,
          params.fieldBoosts,
          params.datatypeBoosts,
          Map(1 -> 0.145.toFloat), // domainIdBoosts, will go away if we do a new searchrequest
          params.minShouldMatch,
          params.slop,
          params.offset,
          params.limit,
          params.sortOrder
        )
        logger.info(LogHelper.formatEsRequest(req))
        val res = req.execute.actionGet
        val count = res.getHits.getTotalHits

        // TODO make format method useful for other classes
        val formattedResults: SearchResults[SearchResult] = format(idCnames, params.showScore, res)
        val timings = InternalTimings(Timings.elapsedInMillis(now), Seq(0, res.getTookInMillis))
        logSearchTerm(searchContextDomain, params.searchQuery)

        (formattedResults.copy(resultSetSize = Some(count), timings = Some(timings)), timings, setCookies)
    } else {

    }
  }
}

//  this will hit doSearch
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
      case NonFatal(e) =>
        val msg = "Cetera profile service error"
        val esError = ElasticsearchError(e)
        logger.error(s"$msg: $esError")
        InternalServerError ~> HeaderAclAllowOriginAll ~> jsonError("We're sorry. Something went wrong.")
    }
  }

  object Service extends SimpleResource {
    override def get: HttpService = search
  }

}
