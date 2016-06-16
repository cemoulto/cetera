package com.socrata.cetera.services

import scala.util.control.NonFatal

import com.socrata.http.server._
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses._
import com.socrata.http.server.routing.SimpleResource
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.authentication.CoreClient
import com.socrata.cetera.search.{DomainClient, _}
import com.socrata.cetera.services.SearchService.format
import com.socrata.cetera.types._
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util._

class ProfileService(elasticSearchClient: BaseDocumentClient,
                     domainClient: DomainClient,
                     coreClient: CoreClient) extends SimpleResource {
  lazy val logger = LoggerFactory.getLogger(classOf[SearchService])

  // TODO possibly consolidate auth into one place (this func template pulled from UserService)
  def verifyUserAuthentication(
                                cookie: Option[String],
                                domainCname: String,
                                requestId: Option[String])
  : (Boolean, Seq[String]) = {
    cookie match {
      case Some(c) =>
        val (authUser, setCookies) = coreClient.fetchUserByCookie(domainCname, c, requestId)
        (authUser.nonEmpty, setCookies)
      case _ =>
        (false, Seq.empty[String])
    }
  }

  // TODO consolidate this with same function in UserSearchService
  def fetchDomainAndUserAuthentication(
                                       domainCname: Option[String],
                                       cookie: Option[String],
                                       requestId: Option[String])
  : (Option[Domain], Long, Boolean, Seq[String]) =
    domainCname.map { extendedHost =>
      val (domainFound, domainTime) = domainClient.find(extendedHost)
      domainFound.map { domain: Domain =>
        val (authorized, setCookies) = verifyUserAuthentication(cookie, domain.domainCname, requestId)
        (domainFound, domainTime, authorized, setCookies)
      }.getOrElse((None, domainTime, false, Seq.empty[String]))
    }.getOrElse((None, 0L, false, Seq.empty[String]))

  def doSearch(queryParameters: MultiQueryParams, // scalastyle:ignore parameter.number method.length
               cookie: Option[String],
               extendedHost: Option[String],
               requestId: Option[String]
              ): (StatusResponse, SearchResults[SearchResult], InternalTimings, Seq[String]) = {
    val now = Timings.now()

    val (domainFound, domainSearchTime, authorized, setCookies) =
      fetchDomainAndUserAuthentication(extendedHost, cookie, requestId)

    if (authorized) {
      QueryParametersParser(queryParameters, extendedHost) match {
        case Left(errors) =>
          val msg = errors.map(_.message).mkString(", ")
          throw new IllegalArgumentException(s"Invalid query parameters: $msg")

        case Right(params) =>
          domainFound match {
            case Some(domain) =>
              val req = elasticSearchClient.buildPersonalCatalogRequest(
                params.user,
                params.recipient,
                params.searchQuery,
                domainFound.getOrElse(throw new IllegalArgumentException("Search context must be specified")),
                params.categories,
                params.tags,
                params.domainMetadata,
                params.datatypes,
                params.attribution,
                params.parentDatasetId,
                params.fieldBoosts,
                params.datatypeBoosts,
                params.minShouldMatch,
                params.slop,
                params.offset,
                params.limit,
                params.sortOrder
              )

              logger.info(LogHelper.formatEsRequest(req))
              val res = req.execute.actionGet
              val count = res.getHits.getTotalHits

              // TODO refactor so we don't need to pull in SearchService's format method
              val idCnames = Map(domain.domainId -> domain.domainCname)
              val formattedResults: SearchResults[SearchResult] = format(idCnames, params.showScore, res)
              val timings = InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime, res.getTookInMillis))

              (OK, formattedResults.copy(timings = Some(timings)), timings, setCookies)

            case None =>
              logger.error("No domain found. How would this even happen?")
              throw new DomainNotFound(extendedHost.getOrElse("None"))
          }
        }
      } else {
        logger.warn(s"user failed authorization $cookie $extendedHost $requestId")
        (Unauthorized, SearchResults(Seq.empty[SearchResult], 0),
          InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime)), setCookies)
      }
  }

  def search(req: HttpRequest): HttpResponse = {
    logger.debug(LogHelper.formatHttpRequestVerbose(req))

    val cookie = req.header(HeaderCookieKey)
    val extendedHost = req.header(HeaderXSocrataHostKey)
    val requestId = req.header(HeaderXSocrataRequestIdKey)

    try {
      val (status, formattedResults, timings, setCookies) =
        doSearch(req.multiQueryParams, cookie, extendedHost, requestId)
      logger.info(LogHelper.formatRequest(req, timings))
      Http.decorate(Json(formattedResults, pretty = true), status, setCookies)
    } catch {
      case e: IllegalArgumentException =>
        logger.info(e.getMessage)
        BadRequest ~> HeaderAclAllowOriginAll ~> jsonError(e.getMessage)
      case DomainNotFound(e) =>
        val msg = s"Domain not found: $e"
        logger.error(msg)
        NotFound ~> HeaderAclAllowOriginAll ~> jsonError(msg)
      case NonFatal(e) =>
        val msg = "Cetera profile service error"
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
