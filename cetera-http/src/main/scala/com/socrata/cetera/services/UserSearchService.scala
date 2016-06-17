package com.socrata.cetera.services

import scala.util.control.NonFatal

import com.socrata.http.server._
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses._
import com.socrata.http.server.routing.SimpleResource
import org.slf4j.LoggerFactory
import com.socrata.cetera._
import com.socrata.cetera.auth.{CoreClient, User}
import com.socrata.cetera.auth.VerificationClient
import com.socrata.cetera.search.{DomainClient, UserClient}
import com.socrata.cetera.types._
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util.{InternalTimings, SearchResults, Timings, _}

class UserSearchService(userClient: UserClient, coreClient: CoreClient, domainClient: DomainClient) {
  lazy val logger = LoggerFactory.getLogger(getClass)
  val verificationClient = new VerificationClient(domainClient, coreClient)

  def doSearch(
      queryParameters: MultiQueryParams,
      cookie: Option[String],
      extendedHost: Option[String],
      requestId: Option[String])
  : (StatusResponse, SearchResults[DomainUser], InternalTimings, Seq[String]) = {
    val now = Timings.now()

    val (domain, domainSearchTime, authorized, setCookies) =
      verificationClient.fetchDomainAndUserAuthorization(extendedHost, cookie, requestId, {u: User => u.canViewUsers})

    val (status, results: SearchResults[DomainUser], timings) =
      if (authorized) {
        val query = queryParameters.getOrElse("q", Seq.empty).headOption
        val (results: Set[EsUser], userSearchTime) = userClient.search(query)

        val formattedResults = SearchResults[DomainUser](results.toSeq.flatMap(u => DomainUser(domain, u)),
          results.size)
        val timings = InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime, userSearchTime))

        (OK, formattedResults.copy(timings = Some(timings)), timings)
      } else {
        logger.warn(s"user failed authorization $cookie $extendedHost $requestId")
        (Unauthorized, SearchResults(Seq.empty, 0),
          InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime)))
      }

    (status, results, timings, setCookies)
  }

  // $COVERAGE-OFF$ jetty wiring
  def search(req: HttpRequest): HttpResponse = {
    logger.debug(LogHelper.formatHttpRequestVerbose(req))

    val cookie = req.header(HeaderCookieKey)
    val extendedHost = req.header(HeaderXSocrataHostKey)
    val requestId = req.header(HeaderXSocrataRequestIdKey)

    try {
      val (status, results, timings, setCookies) = doSearch(req.multiQueryParams, cookie, extendedHost, requestId)
      logger.info(LogHelper.formatRequest(req, timings))
      Http.decorate(Json(results, pretty = true), status, setCookies)
    } catch {
      case e: IllegalArgumentException =>
        logger.info(e.getMessage)
        BadRequest ~> HeaderAclAllowOriginAll ~> jsonError(e.getMessage)
      case NonFatal(e) =>
        val msg = "Cetera search service error"
        val esError = ElasticsearchError(e)
        logger.error(s"$msg: $esError")
        InternalServerError ~> HeaderAclAllowOriginAll ~> jsonError("We're sorry. Something went wrong.")
    }
  }

  object Service extends SimpleResource {
    override def get: HttpService = search
  }
  // $COVERAGE-ON$
}
