package com.socrata.cetera.util

import java.io.{StringWriter, PrintWriter}

import com.rojoma.json.v3.codec.JsonEncode
import com.rojoma.json.v3.util.{AutomaticJsonCodecBuilder, AutomaticJsonEncodeBuilder}
import com.socrata.http.server.HttpResponse
import com.socrata.http.server.responses.Json

object JsonResponses {
  def jsonError(error: String): HttpResponse = {
    val errorMap = Map("error" -> error)
    Json(errorMap)
  }

  def jsonError(context: String, error: Throwable): HttpResponse = {
    Json(Map("context" -> context,
      "error" -> error.toString,
      "stackTrace" -> getStackTraceAsString(error)
    ))}

  private

  // from http://alvinalexander.com/scala/how-convert-stack-trace-exception-string-print-logger-logging-log4j-slf4j
  def getStackTraceAsString(t: Throwable) = {
    val sw = new StringWriter
    t.printStackTrace(new PrintWriter(sw))
    sw.toString
  }
}

// Helpers for internal timing information to report to the caller
case class InternalTimings(serviceMillis: Long, searchMillis: Seq[Long])

object InternalTimings {
  implicit val jCodec = AutomaticJsonCodecBuilder[InternalTimings]
}

case class SearchResults[T](results: Seq[T],
                            resultSetSize: Long,
                            timings: Option[InternalTimings] = None)

object SearchResults {
  implicit def jEncode[T : JsonEncode]: JsonEncode[SearchResults[T]] = {
    AutomaticJsonEncodeBuilder[SearchResults[T]]
  }
}
