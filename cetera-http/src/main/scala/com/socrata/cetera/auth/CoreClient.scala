package com.socrata.cetera.auth

import scala.util.control.NonFatal

import com.rojoma.simplearm.v2.{ResourceScope, using}
import com.socrata.http.client.{HttpClientHttpClient, RequestBuilder}
import org.apache.http.HttpStatus._
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.util.LogHelper

class CoreClient(httpClient: HttpClientHttpClient, host: String, port: Int,
                 connectTimeoutMs: Int, appToken: Option[String]) {

  val logger = LoggerFactory.getLogger(getClass)
  val headers = List(("Content-Type", "application/json; charset=utf-8")) ++
    appToken.map(("X-App-Token", _))

  val coreBaseRequest = RequestBuilder(host, secure = false)
    .port(port)
    .connectTimeoutMS(connectTimeoutMs)
    .addHeaders(headers)

  private def coreRequest(paths: Iterable[String],
                          params: Map[String, String],
                          domain: Option[String] = None,
                          cookie: Option[String] = None,
                          requestId: Option[String] = None
                         ): RequestBuilder = {
    val headers = List(
      domain.map((HeaderXSocrataHostKey, _)),
      cookie.map((HeaderCookieKey, _)),
      requestId.map((HeaderXSocrataRequestIdKey, _))
    ).flatten

    coreBaseRequest
      .addPaths(paths)
      .addParameters(params)
      .addHeaders(headers)
  }

  def optionallyGetUserByCookie(domain: Option[String],
                                cookie: Option[String],
                                requestId: Option[String]
                               ): (Option[User], Seq[String]) = {
    (domain, cookie) match {
      case (Some(cname), None) =>
        logger.warn("Search context was provided without cookie. Not validating user.")
        (None, Seq.empty)
      case (None, Some(nomNom)) =>
        logger.warn("Cookie provided without search context. Not validating user.")
        (None, Seq.empty)
      case (Some(cname), Some(nomNom)) => fetchUserByCookie(cname, nomNom, requestId)
      case (None, None) => (None, Seq.empty)
    }
  }

  def fetchUserByCookie(domain: String, cookie: String, requestId: Option[String]): (Option[User], Seq[String]) = {
    if (cookie.nonEmpty) {
      val req = coreRequest(List("users.json"), Map("method" -> "getCurrent"), Some(domain), Some(cookie), requestId)

      logger.info(s"Validating user on domain $domain")
      logger.debug(LogHelper.formatSimpleHttpRequestBuilderVerbose(req))
      using(new ResourceScope(s"retrieving current user from core")) { rs =>
        try {
          val res = httpClient.execute(req.get, rs)
          val setCookies = res.headers(HeaderSetCookieKey).toSeq
          res.resultCode match {
            case SC_OK => res.value[User]() match {
              case Right(u) => (Some(u), setCookies)
              case Left(err) =>
                logger.error(s"Could not parse core user data: \n ${err.english}")
                (None, setCookies)
            }
            case SC_FORBIDDEN | SC_UNAUTHORIZED =>
              logger.warn(s"User is unauthorized on $domain.")
              (None, setCookies)
            case code: Int =>
              logger.warn(s"Could not validate cookie with core. Core returned a $code.")
              (None, setCookies)
          }
        } catch {
          case NonFatal(e) =>
            logger.error("Cannot reach core to validate cookie")
            (None, Seq.empty)
        }
      }
    } else {
      (None, Seq.empty)
    }
  }

  def fetchUserById(domain: String, id: String, requestId: Option[String]): (Option[User], Seq[String]) = {
    val req = coreRequest(List("users", id), Map.empty, Some(domain), None, requestId)

    logger.info(s"Looking up user $id on domain $domain")
    logger.debug(LogHelper.formatSimpleHttpRequestBuilderVerbose(req))
    using(new ResourceScope(s"retrieving user from core")) { rs =>
      try {
        val res = httpClient.execute(req.get, rs)
        val setCookies = res.headers(HeaderSetCookieKey).toSeq
        res.resultCode match {
          case SC_OK => res.value[User]() match {
            case Right(u) => (Some(u), setCookies)
            case Left(err) =>
              logger.error(s"Could not parse core data for user $id: \n ${err.english}")
              (None, setCookies)
          }
          case code: Int =>
            logger.warn(s"Could not fetch user $id from core. Core returned a $code.")
            (None, setCookies)
        }
      } catch {
        case NonFatal(e) =>
          logger.error(s"Cannot reach core to fetch user $id")
          (None, Seq.empty)
      }
    }
  }
}
