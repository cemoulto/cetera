package com.socrata.cetera.services

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

import com.socrata.http.server.implicits._
import com.socrata.http.server.responses.{BadRequest, InternalServerError, Json, OK}
import com.socrata.http.server.routing.SimpleResource
import com.socrata.http.server.{HttpRequest, HttpResponse, HttpService}
import org.elasticsearch.search.aggregations.bucket.filter.Filter
import org.elasticsearch.search.aggregations.bucket.nested.Nested
import org.elasticsearch.search.aggregations.bucket.terms.Terms
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.search.{DocumentClient, DomainClient, DomainNotFound}
import com.socrata.cetera.types._
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util._

class FacetService(documentClient: DocumentClient, domainClient: DomainClient) {
  lazy val logger = LoggerFactory.getLogger(classOf[FacetService])

  // $COVERAGE-OFF$ jetty wiring
  case class Service(cname: String) extends SimpleResource {
    override def get: HttpService = aggregate(cname)
  }

  def aggregate(cname: String)(req: HttpRequest): HttpResponse = {
    QueryParametersParser(req) match {
      case Left(errors) =>
        val msg = errors.map(_.message).mkString(", ")
        BadRequest ~> HeaderAclAllowOriginAll ~> jsonError(s"Invalid query parameters: $msg")
      case Right(params) =>
        try {
          val (facets, timings) = doAggregate(cname, req.header("Cookie"))
          logger.info(LogHelper.formatRequest(req, timings))
          OK ~> HeaderAclAllowOriginAll ~> Json(facets)
        } catch {
          case NonFatal(e) =>
            val esError = ElasticsearchError(e)
            logger.error(s"Database error: ${esError.getMessage}")
            InternalServerError ~> HeaderAclAllowOriginAll ~> jsonError(s"Database error", esError)
        }
    }
  }
  // $COVERAGE-ON$

  def doAggregate(cname: String, cookie: Option[String]): (Seq[FacetCount], InternalTimings) = {
    val startMs = Timings.now()

    val (domain, domainSearchTime) = domainClient.findRelevantDomains(Some(cname), Set(cname), cookie)

    domain.headOption.getOrElse(throw new DomainNotFound(cname))

    val request = documentClient.buildFacetRequest(domain.headOption)
    logger.info(LogHelper.formatEsRequest(request))
    val res = request.execute().actionGet()
    val aggs = res.getAggregations.asMap().asScala
      .getOrElse("domain_filter", throw new NoSuchElementException).asInstanceOf[Filter]
      .getAggregations.asMap().asScala

    val datatypesValues = aggs("datatypes").asInstanceOf[Terms]
      .getBuckets.asScala.map(b => ValueCount(b.getKey, b.getDocCount)).toSeq.filter(_.value.nonEmpty)
    val datatypesFacets = Seq(FacetCount("datatypes", datatypesValues.map(_.count).sum, datatypesValues))

    val categoriesValues = aggs("categories").asInstanceOf[Terms]
      .getBuckets.asScala.map(b => ValueCount(b.getKey, b.getDocCount)).toSeq.filter(_.value.nonEmpty)
    val categoriesFacets = Seq(FacetCount("categories", categoriesValues.map(_.count).sum, categoriesValues))

    val tagsValues = aggs("tags").asInstanceOf[Terms]
      .getBuckets.asScala.map(b => ValueCount(b.getKey, b.getDocCount)).toSeq.filter(_.value.nonEmpty)
    val tagsFacets = Seq(FacetCount("tags", tagsValues.map(_.count).sum, tagsValues))

    val metadataFacets = aggs("metadata").asInstanceOf[Nested]
      .getAggregations.get("keys").asInstanceOf[Terms]
      .getBuckets.asScala.map { b =>
        val values = b.getAggregations.get("values").asInstanceOf[Terms]
          .getBuckets.asScala.map { v => ValueCount(v.getKey, v.getDocCount) }.toSeq
        FacetCount(b.getKey, b.getDocCount, values)
      }.toSeq

    val facets: Seq[FacetCount] = Seq.concat(datatypesFacets, categoriesFacets, tagsFacets, metadataFacets)
    val timings = InternalTimings(Timings.elapsedInMillis(startMs), Seq(domainSearchTime, res.getTookInMillis))
    (facets, timings)
  }
}
