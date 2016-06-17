package com.socrata.cetera.util

import javax.servlet.http.HttpServletResponse

import com.socrata.http.server.HttpResponse
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses._
import org.slf4j.LoggerFactory

import com.socrata.cetera._

object Http {
  lazy val logger = LoggerFactory.getLogger(Http.getClass)

  def decorate(content: HttpResponse, status: StatusResponse, setCookies: Seq[String]): HttpResponse = {
    val debugMessage = StringBuilder.newBuilder
    debugMessage.append(s"sending http response with (partial) headers:\n")

    val baseResponse: HttpResponse = status ~> HeaderAclAllowOriginAll
    debugMessage.append(s"HTTP ${status.statusCode}\n")
    debugMessage.append("Access-Control-Allow-Origin: *\n")

    // It would really be nice if socrata-http assembled a chained http request with multiple cookies.
    //
    // Someday we might want to secure cookies coming out of catalog api, like frontend does now,
    // but only if we enable and require HTTPS connections.
    // See https://en.wikipedia.org/wiki/HTTP_cookie#Cross-site_scripting_.E2.80.93_cookie_theft
    //
    // Additionally, frontend code limits the cookies in forwardable_session_cookies; another time perhaps.
    val responseWithCookies = setCookies.foldLeft(baseResponse){ case (r, s) =>
      debugMessage.append(s"$HeaderSetCookieKey: $s\n")
      r.compose[HttpServletResponse] { sr =>
        sr.addHeader(HeaderSetCookieKey, s)
        sr
      }
    }

    logger.debug(debugMessage.toString())
    responseWithCookies ~> content
  }
}
