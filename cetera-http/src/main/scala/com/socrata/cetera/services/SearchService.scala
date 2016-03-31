package com.socrata.cetera.services

import scala.util.control.NonFatal

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.codec.DecodeError
import com.rojoma.json.v3.io.JsonReader
import com.rojoma.json.v3.jpath.JPath
import com.rojoma.json.v3.util.{AutomaticJsonCodecBuilder, JsonKeyStrategy, Strategy}
import com.socrata.http.server.implicits._
import com.socrata.http.server.responses._
import com.socrata.http.server.routing.SimpleResource
import com.socrata.http.server.{HttpRequest, HttpResponse, HttpService}
import org.elasticsearch.action.search.SearchResponse
import org.elasticsearch.search.SearchHits
import org.slf4j.LoggerFactory

import com.socrata.cetera._
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.search.{DocumentClient, DomainClient}
import com.socrata.cetera.types._
import com.socrata.cetera.util.JsonResponses._
import com.socrata.cetera.util._

@JsonKeyStrategy(Strategy.Underscore)
case class Classification(categories: Seq[JValue],
                          tags: Seq[JValue],
                          domainCategory: Option[JValue],
                          domainTags: Option[JValue],
                          domainMetadata: Option[JValue])

object Classification {
  implicit val jCodec = AutomaticJsonCodecBuilder[Classification]
}

case class SearchResult(resource: JValue,
                        classification: Classification,
                        metadata: Map[String, JValue],
                        permalink: JString,
                        link: JString)

object SearchResult {
  implicit val jCodec = AutomaticJsonCodecBuilder[SearchResult]
}

class SearchService(elasticSearchClient: DocumentClient,
                    domainClient: DomainClient,
                    balboaClient: BalboaClient) extends SimpleResource {
  lazy val logger = LoggerFactory.getLogger(classOf[SearchService])

  // TODO: cetera-etl rename customer_blah to domain_blah
  private def domainCategory(j: JValue): Option[JValue] = j.dyn.customer_category.? match {
    case Left(e) => None
    case Right(jv) => Some(jv)
  }

  private def domainCategoryString(j: JValue): Option[String] =
    domainCategory(j).flatMap {
      case JString(s) => Option(s)
      case _ => None
    }

  private def domainTags(j: JValue): Option[JValue] = j.dyn.customer_tags.? match {
    case Left(e) => None
    case Right(jv) => Some(jv)
  }

  private def domainMetadata(j: JValue): Option[JValue] = j.dyn.customer_metadata_flattened.? match {
    case Left(e) => None
    case Right(jv) => Some(jv)
  }

  private def categories(j: JValue): Stream[JValue] =
    new JPath(j).down("animl_annotations").down("categories").*.down("name").finish.distinct

  private def tags(j: JValue): Stream[JValue] =
    new JPath(j).down("animl_annotations").down("tags").*.down("name").finish.distinct

  private def popularity(j: JValue): Option[(String,JValue)] =
    j.dyn.popularity.?.fold(_ => None, r => Option(("popularity", r)))

  private def updateFreq(j: JValue): Option[(String,JValue)] =
    j.dyn.update_freq.?.fold(_ => None, r => Option(("update_freq", r)))

  private def cname(domainCnames: Map[Int,String], j: JValue): String = {
    val id: Option[Int] = j.dyn.socrata_id.domain_id.! match {
      case jn: JNumber => Option(jn.toInt)
      case JArray(elems) => elems.lastOption.map(_.asInstanceOf[JNumber].toInt)
      case jv: JValue => throw new NoSuchElementException(s"Unexpected json value $jv")
    }
    id.flatMap { i =>
      domainCnames.get(i)
    }.getOrElse("") // if no domain was found, default to blank string
  }

  private def extractJString(decoded: Either[DecodeError, JValue]): Option[String] =
    decoded.fold(_ => None, {
      case JString(s) => Option(s)
      case _ => None
    })

  private def datatype(j: JValue): Option[Datatype] =
    extractJString(j.dyn.datatype.?).flatMap(s => Datatype(s))

  private def viewtype(j: JValue): Option[String] = extractJString(j.dyn.viewtype.?)

  private def datasetId(j: JValue): Option[String] = extractJString(j.dyn.socrata_id.dataset_id.?)

  private def datasetName(j: JValue): Option[String] = extractJString(j.dyn.resource.name.?)

  private def fetchDomainCnames(domainIdCnames: Map[Int, String], hits: SearchHits): Map[Int, String] = {
    val distinctDomainIds = hits.hits.map { h =>
      JsonReader.fromString(h.sourceAsString()).dyn.socrata_id.domain_id.! match {
        case jn: JNumber => jn.toInt
        case _ => throw new scala.NoSuchElementException
      }
    }.toSet // deduplicate domain id

    val unknownDomainIds = distinctDomainIds -- domainIdCnames.keys // don't repeat lookup for known domains
    if (unknownDomainIds.nonEmpty) logger.warn(s"Domains not known at query construction time: $unknownDomainIds.")
    unknownDomainIds.flatMap { i =>
      domainClient.fetch(i).map { d => i -> d.domainCname } // lookup domain cname from elasticsearch
    }.toMap ++ domainIdCnames
  }

  def format(domainCnames: Map[Int,String], showScore: Boolean,
             searchResponse: SearchResponse): SearchResults[SearchResult] = {
    SearchResults(searchResponse.getHits.hits().map { hit =>
      val json = JsonReader.fromString(hit.sourceAsString())

      val score = if (showScore) Seq("score" -> JNumber(hit.score)) else Seq.empty
      val links = SearchService.links(
        cname(domainCnames, json),
        datatype(json),
        viewtype(json),
        datasetId(json).getOrElse(throw new NoSuchElementException),
        domainCategoryString(json),
        datasetName(json).getOrElse(throw new NoSuchElementException))

      SearchResult(
        json.dyn.resource.!,
        Classification(
          categories(json),
          tags(json),
          domainCategory(json),
          domainTags(json),
          domainMetadata(json)),
        Map(esDomainType -> JString(cname(domainCnames, json))) ++ score,
        links.getOrElse("permalink", JString("")),
        links.getOrElse("link", JString(""))
      )
    })
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

  def doSearch(queryParameters: MultiQueryParams, cookie: Option[String]): (SearchResults[SearchResult], InternalTimings) = {
    val now = Timings.now()

    QueryParametersParser(queryParameters) match {
      case Left(errors) =>
        val msg = errors.map(_.message).mkString(", ")
        throw new IllegalArgumentException(s"Invalid query parameters: $msg")

      case Right(params) =>
        val (relevantDomains, domainSearchTime) =
          domainClient.findRelevantDomains(params.searchContext, params.domains, cookie)
        val searchContext = params.searchContext.flatMap(cname => relevantDomains.find(_.domainCname == cname))
        val queryDomains = params.domains match {
          case cs: Set[String] if cs.nonEmpty => cs.flatMap (cname => relevantDomains.find (_.domainCname == cname) )
          case _ => relevantDomains
        }

        val domainIdBoosts = params.domainBoosts.flatMap { case (cname: String, weight: Float) =>
          relevantDomains.collect { case d: Domain if d.domainCname == cname => d.domainId -> weight }
        }
        val domainIdCnames = relevantDomains.map(d => d.domainId -> d.domainCname).toMap

        val req = elasticSearchClient.buildSearchRequest(
          params.searchQuery,
          queryDomains,
          params.domainMetadata,
          searchContext,
          params.categories,
          params.tags,
          params.only,
          params.fieldBoosts,
          params.datatypeBoosts,
          domainIdBoosts,
          params.minShouldMatch,
          params.slop,
          params.offset,
          params.limit,
          params.sortOrder
        )

        logger.info(LogHelper.formatEsRequest(req))

        val res = req.execute.actionGet
        val count = res.getHits.getTotalHits
        val cnames = fetchDomainCnames(domainIdCnames, res.getHits)
        val formattedResults: SearchResults[SearchResult] = format(cnames, params.showScore, res)
        val timings = InternalTimings(Timings.elapsedInMillis(now), Seq(domainSearchTime, res.getTookInMillis))
        logSearchTerm(searchContext, params.searchQuery)

        (formattedResults.copy(resultSetSize = Some(count), timings = Some(timings)), timings)
    }
  }

  // $COVERAGE-OFF$ jetty wiring
  def search(req: HttpRequest): HttpResponse = {
    try {
      val (formattedResults, timings) = doSearch(req.multiQueryParams, req.header("Cookie"))
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
    override def get: HttpService = search
  }
  // $COVERAGE-ON$
}

object SearchService {
  def links(cname: String,
             datatype: Option[Datatype],
             viewtype: Option[String],
             datasetId: String,
             datasetCategory: Option[String],
             datasetName: String): Map[String,JString] = {
    val perma = (datatype, viewtype) match {
      case (Some(TypeStories), _)             => s"stories/s"
      case (Some(TypeDatalenses), _)          => s"view"
      case (_, Some(TypeDatalenses.singular)) => s"view"
      case _                                  => s"d"
    }

    val urlSegmentLengthLimit = 50
    def hyphenize(text: String): String = Option(text) match {
      case Some(s) if s.nonEmpty => s.replaceAll("[^\\p{L}\\p{N}_]+", "-").take(urlSegmentLengthLimit)
      case _ => "-"
    }
    val pretty = datatype match {
      // TODO: maybe someday stories will allow pretty seo links
      // stories don't have a viewtype today, but who knows...
      case Some(TypeStories) => perma
      case _ =>
        val category = datasetCategory.filter(s => s.nonEmpty).getOrElse(TypeDatasets.singular)
        s"${hyphenize(category)}/${hyphenize(datasetName)}"
    }

    Map(
      "permalink" ->JString(s"https://$cname/$perma/$datasetId"),
      "link" -> JString(s"https://$cname/$pretty/$datasetId")
    )
  }
}
