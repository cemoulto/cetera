package com.socrata.cetera.services

import javax.servlet.http.HttpServletResponse
import scala.util.{Try, Success, Failure}

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.io.JsonReader
import com.rojoma.json.v3.jpath.JPath
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses._
import com.socrata.http.server.routing.SimpleResource
import com.socrata.http.server.{HttpRequest, HttpService}
import org.elasticsearch.ElasticsearchException
import org.elasticsearch.action.search.SearchResponse
import org.slf4j.LoggerFactory

import com.socrata.cetera.search.ElasticSearchClient
import com.socrata.cetera.types._
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util._

class CountService(elasticSearchClient: ElasticSearchClient) {
  lazy val logger = LoggerFactory.getLogger(classOf[CountService])

  // Possibly belongs in the client
  // Fails silently if path does not exist
  def extract(body: JValue): Stream[JValue] = {
    val jPath = new JPath(body)
    jPath
      .down("aggregations")
      .down("counts")
      .down("buckets")
      .*
      .finish
  }

  // Unhandled exception on missing key
  def format(counts: Stream[JValue]): SearchResults[Count] =  {
    SearchResults(
      counts.map { c => Count(c.dyn.key.!, c.dyn.doc_count.!) }
    )
  }

  def aggregate(field: CeteraFieldType with Countable)(req: HttpRequest): HttpServletResponse => Unit = {
    val now = Timings.now()

    implicit val cEncode = field match {
      case DomainFieldType => Count.encode("domain")
      case CategoriesFieldType => Count.encode("category")
      case TagsFieldType => Count.encode("tag")
    }

    QueryParametersParser(req) match {
      case Right(params) =>
        val request = elasticSearchClient.buildCountRequest(
          field,
          params.searchQuery,
          params.domains,
          params.categories,
          params.tags,
          params.only
        )

        val response = Try(request.execute().actionGet())

        response match {
          case Success(res) =>
            val timings = InternalTimings(Timings.elapsedInMillis(now), Option(res.getTookInMillis()))

            val json = JsonReader.fromString(res.toString)
            val counts = extract(json)
            val formattedResults = format(counts).copy(timings = Some(timings))

            val logMsg = LogHelper.formatRequest(req, timings)
            logger.info(logMsg)

            val payload = Json(formattedResults, pretty=true)
            OK ~> Header("Access-Control-Allow-Origin", "*") ~> payload

          case Failure(ex) =>
            InternalServerError ~> Header("Access-Control-Allow-Origin", "*") ~> jsonError(s"Database error: ${ex.getMessage}")
        }

      case Left(errors) =>
        val msg = errors.map(_.message).mkString(", ")
        BadRequest ~> Header("Access-Control-Allow-Origin", "*") ~> jsonError(s"Invalid query parameters: ${msg}")
    }
  }

  case class Service(field: CeteraFieldType with Countable) extends SimpleResource {
    override def get: HttpService = aggregate(field)
  }
}