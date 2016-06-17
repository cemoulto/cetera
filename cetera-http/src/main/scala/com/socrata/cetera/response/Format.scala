package com.socrata.cetera.response

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.codec.DecodeError
import com.rojoma.json.v3.io.JsonReader
import com.rojoma.json.v3.jpath.JPath
import com.rojoma.json.v3.util.{AutomaticJsonCodecBuilder, JsonKeyStrategy, Strategy}
import com.socrata.cetera._
import com.socrata.cetera.types._
import com.socrata.cetera.util._
import org.elasticsearch.action.search.SearchResponse
import org.slf4j.LoggerFactory

@JsonKeyStrategy(Strategy.Underscore)
case class Classification(categories: Seq[JValue],
                          tags: Seq[JValue],
                          domainCategory: Option[JValue],
                          domainTags: Option[JValue],
                          domainMetadata: Option[JValue])

object Classification {
  implicit val jCodec = AutomaticJsonCodecBuilder[Classification]
}

case class DocumentSearchResult(resource: JValue,
                                classification: Classification,
                                metadata: Map[String, JValue],
                                permalink: JString,
                                link: JString)

object DocumentSearchResult {
  implicit val jCodec = AutomaticJsonCodecBuilder[DocumentSearchResult]
}

object Format {
  lazy val logger = LoggerFactory.getLogger(Format.getClass)

  private def links(cname: String,
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

  // WARN: This will raise if a single document has a single missing path!
  def formatDocumentResponse(domainIdCnames: Map[Int, String],
             showScore: Boolean,
             searchResponse: SearchResponse): SearchResults[DocumentSearchResult] = {
    val hits = searchResponse.getHits
    val searchResult = hits.hits().map { hit =>
      val json = JsonReader.fromString(hit.sourceAsString())

      val score = if (showScore) Seq("score" -> JNumber(hit.score)) else Seq.empty
      val linkMap = links(
        cname(domainIdCnames, json),
        datatype(json),
        viewtype(json),
        datasetId(json).get,
        domainCategoryString(json),
        datasetName(json).get)

      DocumentSearchResult(
        json.dyn.resource.!,
        Classification(
          categories(json),
          tags(json),
          domainCategory(json),
          domainTags(json),
          domainMetadata(json)),
        Map(esDomainType -> JString(cname(domainIdCnames, json))) ++ score,
        linkMap.getOrElse("permalink", JString("")),
        linkMap.getOrElse("link", JString(""))
      )
    }
    SearchResults(searchResult, hits.getTotalHits)
  }
}
