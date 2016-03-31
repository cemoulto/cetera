package com.socrata.cetera.services

import scala.util.control.NonFatal

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.codec.DecodeError
import com.rojoma.json.v3.io.JsonReader
import com.rojoma.json.v3.matcher.{PObject, Variable}
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses.{BadRequest, InternalServerError, Json, OK}
import com.socrata.http.server.routing.SimpleResource
import com.socrata.http.server.{HttpRequest, HttpResponse, HttpService}
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.search.DomainClient
import com.socrata.cetera.types.Count
import com.socrata.cetera.util.JsonResponses.jsonError
import com.socrata.cetera.util._

class DomainCountService(domainClient: DomainClient) {
  lazy val logger = LoggerFactory.getLogger(classOf[CountService])

  private def extract(json: JValue): Either[DecodeError, Seq[JValue]] = {
    val buckets = Variable.decodeOnly[Seq[JValue]]
    val pattern = PObject(
      "aggregations" -> PObject(
        "domains" -> PObject(
          "buckets" -> buckets
        )
      )
    )

    pattern.matches(json).right.map(buckets)
  }

  private def format(counts: Seq[JValue]): SearchResults[Count] =
    SearchResults(counts.map { c => Count(c.dyn.key.!, c.dyn.documents.visible.doc_count.!) })

  def doAggregate(queryParameters: MultiQueryParams, cookie: Option[String]): (SearchResults[Count], InternalTimings) = {
    val now = Timings.now()

    QueryParametersParser(queryParameters) match {
      case Left(errors) =>
        val msg = errors.map(_.message).mkString(", ")
        throw new IllegalArgumentException(s"Invalid query parameters: $msg")

      case Right(params) =>
        val (relevantDomains, domainSearchTime) =
          domainClient.findRelevantDomains(params.searchContext, params.domains, cookie)
        val searchContext = params.searchContext.flatMap(cname => relevantDomains.find(_.domainCname == cname))

        val search = domainClient.buildCountRequest(
          params.searchQuery,
          relevantDomains,
          searchContext,
          params.categories,
          params.tags,
          params.only
        )
        logger.info(LogHelper.formatEsRequest(search))
        val res = search.execute.actionGet
        val timings = InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime, res.getTookInMillis))
        val json = JsonReader.fromString(res.toString)
        val counts = extract(json) match {
          case Right(extracted) => extracted
          case Left(error) =>
            logger.error(error.english)
            throw new JsonDecodeException(error)
        }
        val formattedResults: SearchResults[Count] = format(counts).copy(timings = Some(timings))
        (formattedResults, timings)
    }
  }

  // $COVERAGE-OFF$ jetty wiring
  def aggregate()(req: HttpRequest): HttpResponse = {
    implicit val cEncode = Count.encode(esDomainType)

    try {
      val (formattedResults, timings) = doAggregate(req.multiQueryParams, req.header("Cookie"))
      logger.info(LogHelper.formatRequest(req, timings))
      OK ~> HeaderAclAllowOriginAll ~> Json(formattedResults, pretty = true)
    } catch {
      case e: IllegalArgumentException =>
        logger.info(e.getMessage)
        BadRequest ~> HeaderAclAllowOriginAll ~> jsonError(e.getMessage)
      case NonFatal(e) =>
        val esError = ElasticsearchError(e)
        logger.error(s"Database error: ${esError.getMessage}")
        InternalServerError ~> HeaderAclAllowOriginAll ~> jsonError(s"Database error", esError)
    }
  }

  object Service extends SimpleResource {
    override def get: HttpService = aggregate()
  }
  // $COVERAGE-ON$
}
