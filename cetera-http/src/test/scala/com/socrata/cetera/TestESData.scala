package com.socrata.cetera

import scala.io.Source

import com.rojoma.json.v3.util.JsonUtil
import org.elasticsearch.common.joda.time.DateTime

import com.socrata.cetera.search.ElasticSearchClient
import com.socrata.cetera.types._
import com.socrata.cetera.util.ElasticsearchBootstrap

trait TestESData {
  val client: ElasticSearchClient
  val testSuiteName: String = getClass.getSimpleName.toLowerCase

  val domains = {
    val domainTSV = Source.fromInputStream(getClass.getResourceAsStream("/domains.tsv"))
    val iter = domainTSV.getLines().map(_.split("\t"))
    iter.drop(1) // drop the header columns
    iter.map { tsvLine =>
      Domain(
        domainId = tsvLine(0).toInt,
        domainCname = tsvLine(1),
        siteTitle = Option(tsvLine(2)).filter(_.nonEmpty),
        organization = Option(tsvLine(3)).filter(_.nonEmpty),
        isCustomerDomain = tsvLine(4).toBoolean,
        moderationEnabled = tsvLine(5).toBoolean,
        routingApprovalEnabled = tsvLine(6).toBoolean,
        lockedDown = tsvLine(7).toBoolean,
        apiLockedDown = tsvLine(8).toBoolean
      )
    }.toSeq
  }

  val users = {
    val userTSV = Source.fromInputStream(getClass.getResourceAsStream("/users.tsv"))
    userTSV.getLines()
    val iter = userTSV.getLines().map(_.split("\t"))
    iter.drop(1) // drop the header columns
    iter.map { tsvLine =>
      EsUser(
        id = tsvLine(0),
        screenName = Option(tsvLine(1)).filter(_.nonEmpty),
        email = Option(tsvLine(2)).filter(_.nonEmpty),
        roles = Some(Set(Role(tsvLine(3).toInt, tsvLine(4)))),
        profileImageUrlLarge = Option(tsvLine(5)).filter(_.nonEmpty),
        profileImageUrlMedium = Option(tsvLine(6)).filter(_.nonEmpty),
        profileImageUrlSmall = Option(tsvLine(7)).filter(_.nonEmpty)
      )
    }.toSeq
  }

  private val esDocTemplate: String =
    """{
      | "socrata_id": {
      |   "dataset_id": %s,
      |   "parent_dataset_id": %s,
      |   "domain_id": %s
      | },
      | "resource": {
      |   "description": %s,
      |   "nbe_fxf": %s,
      |   "parent_fxf": %s,
      |   "updatedAt": %s,
      |   "createdAt": %s,
      |   "type": %s,
      |   "id": %s,
      |   "columns": {
      |     %s
      |   },
      |   "name": %s,
      |   "attribution": %s
      | },
      | "animl_annotations": {
      |   "categories": [
      |     %s
      |   ],
      |   "tags": [
      |     %s
      |   ]
      | },
      | "datatype": %s,
      | "viewtype": %s,
      | "popularity": %s,
      | "indexed_metadata": {
      |   "name": %s,
      |   "description": %s,
      |   "columns_field_name": [
      |     %s
      |   ],
      |   "columns_description": [
      |     %s
      |   ],
      |   "columns_name": [
      |     %s
      |   ]
      | },
      | "customer_metadata_flattened": [
      |   %s
      | ],
      | "is_public": %s,
      | "is_published": %s,
      | "is_default_view": %s,
      | "is_moderation_approved": %s,
      | "approving_domain_ids": [
      |   %s
      | ],
      | "is_approved_by_parent_domain": %s,
      | "page_views": {
      |   "page_views_total": %s
      | },
      | "customer_category": %s,
      | "customer_tags": [
      |   %s
      | ],
      | "update_freq": %s,
      | "owner_id": %s,
      | "owner" : {
      |   "id": %s,
      |   "screen_name": %s
      | },
      | "attribution": %s,
      | "shared_to": %s
      |}
    """.stripMargin

  private def quoteQualify(s: String): String = "\"%s\"".format(s)
  private def quoteQualify(s: Option[String]): String = s.map("\"%s\"".format(_)).getOrElse("null")
  private def quoteQualify(ss: Seq[String]): String = ss.map(quoteQualify).mkString(",\n")
  private def quoteQualifyScore(sm: Map[String,Float]): String = sm.map { kvp =>
    """{ "name":  "%s", "score": %s } """.format(kvp._1, kvp._2.toString)
  }.mkString(",\n")
  private def quoteQualifyMap(sm: Map[String,String]): String = sm.map { kvp =>
    """{ "key": "%s", "value": "%s" }""".format(kvp._1, kvp._2)
  }.mkString(",\n")

  private def buildEsDoc(socrataIdDatasetId: String,
                         parentDatasetId: Option[String],
                         domainId: Int,
                         resourceDescription: String,
                         resourceNbeFxf: String,
                         resourceUpdatedAt: String,
                         resourceCreatedAt: String,
                         resourceId: String,
                         resourceColumns: Seq[String],
                         resourceName: String,
                         resourceAttribution: Option[String],
                         animlAnnotationsCategories: Map[String,Float],
                         animlAnnotationsTags: Map[String,Float],
                         datatype: String,
                         viewtype: String,
                         popularity: Float,
                         indexedMetadataName: String,
                         indexedMetadataDescription: String,
                         indexedMetadataColumnsFieldNames: Seq[String],
                         indexedMetadataColumnsDescriptions: Seq[String],
                         indexedMetadataColumnsNames: Seq[String],
                         customerMetadataFlattened: Map[String,String],
                         isPublic: Boolean,
                         isPublished: Boolean,
                         isDefaultView: Boolean,
                         isModerationApproved: Option[Boolean],
                         approvingDomainIds: Seq[Int],
                         isApprovedByParentDomain: Boolean,
                         pageViewsTotal: String,
                         customerCategory: String,
                         customerTags: Seq[String],
                         updateFreq: Long,
                         ownerId: String,
                         ownerScreenName: String,
                         attribution: Option[String],
                         sharedTo: Option[Seq[String]]): String = {
    val doc = esDocTemplate.format(
      quoteQualify(socrataIdDatasetId),
      quoteQualify(parentDatasetId),
      domainId.toString,
      quoteQualify(resourceDescription),
      quoteQualify(resourceNbeFxf),
      quoteQualify(parentDatasetId),
      quoteQualify(resourceUpdatedAt),
      quoteQualify(resourceCreatedAt),
      quoteQualify(datatype),
      quoteQualify(resourceId),
      quoteQualify(resourceColumns),
      quoteQualify(resourceName),
      quoteQualify(attribution),
      quoteQualifyScore(animlAnnotationsCategories),
      quoteQualifyScore(animlAnnotationsTags),
      quoteQualify(datatype),
      quoteQualify(viewtype),
      popularity.toString,
      quoteQualify(indexedMetadataName),
      quoteQualify(indexedMetadataDescription),
      quoteQualify(indexedMetadataColumnsFieldNames),
      quoteQualify(indexedMetadataColumnsDescriptions),
      quoteQualify(indexedMetadataColumnsNames),
      quoteQualifyMap(customerMetadataFlattened),
      isPublic.toString,
      isPublished.toString,
      isDefaultView.toString,
      isModerationApproved.map(_.toString).getOrElse("null"),
      approvingDomainIds.mkString(","),
      isApprovedByParentDomain.toString,
      pageViewsTotal,
      quoteQualify(customerCategory),
      quoteQualify(customerTags),
      updateFreq.toString,
      quoteQualify(ownerId),
      quoteQualify(ownerId),
      quoteQualify(ownerScreenName),
      quoteQualify(attribution),
      quoteQualify(sharedTo.getOrElse(List("null"))))

    doc
  }

  private def buildEsDocByIndex(i: Int): String = {
    val domainId = i % domainsWithData.length
    val domainApprovalIds = approvingDomainIds(i % approvingDomainIds.length)
    buildEsDoc(
      socrataIdDatasetIds(i % socrataIdDatasetIds.length),
      parentDatasetIds(i % socrataIdDatasetIds.length),
      domainId,
      resourceDescriptions(i % resourceDescriptions.length),
      resourceNbeFxfs(i % resourceNbeFxfs.length),
      defaultResourceUpdatedAt,
      defaultResourceCreatedAt,
      resourceIds(i % resourceIds.length),
      defaultResourceColumns,
      resourceNames(i % resourceNames.length),
      None,
      defaultAaCategories,
      defaultAaTags,
      Datatypes.materialized(i % Datatypes.materialized.length).singular,
      "",
      popularities(i % popularities.length),
      imNames(i % imNames.length),
      imDescriptions(i % imDescriptions.length),
      defaultImColumnsFieldNames,
      defaultImColumnsDescriptions,
      defaultImColumnNames,
      domainMetadata(i % domainMetadata.length),
      isPublics(i % isPublics.length),
      isPublisheds(i % isPublisheds.length),
      isDefaultViews(i % isDefaultViews.length),
      isModerationApproveds(i % isModerationApproveds.length),
      domainApprovalIds,
      domainApprovalIds.contains(domainId),
      pageViewsTotal(i % pageViewsTotal.length),
      domainCategories(i % domainCategories.length),
      domainTags(i % domainTags.length),
      updateFreqs(i % updateFreqs.length),
      ownerIds(i % ownerIds.length),
      ownerScreenNames(i % ownerScreenNames.length),
      attributions(i % attributions.length),
      sharedTos(i % sharedTos.length))
  }

  val domainsWithData = domains.slice(0,4).map(d => d.domainCname)

  val defaultResourceUpdatedAt = DateTime.now().toString
  val defaultResourceCreatedAt = DateTime.now().toString
  val defaultResourceColumns = Nil
  val defaultAaCategories = Map("Personal" -> 1F)
  val defaultAaTags = Map("Happy" -> 0.65F, "Accident" -> 0.35F)
  val defaultImColumnsFieldNames = Nil
  val defaultImColumnsDescriptions = Nil
  val defaultImColumnNames = Nil

  val numericIds = 0 to Datatypes.materialized.length
  val socrataIdDatasetIds = numericIds.map(n => s"fxf-$n")
  val parentDatasetIds = socrataIdDatasetIds.map(s => if (s != "fxf-0") Some("fxf-0") else None)
  val resourceDescriptions = Seq(
    "We're number one",
    "Second the best",
    "Bird is the third word",
    "May the fourth be with you")
  val resourceNbeFxfs = socrataIdDatasetIds
  val resourceIds = socrataIdDatasetIds
  val resourceNames = Seq("One", "Two", "Three", "Four")
  val popularities = Seq(0.1F, 0.42F, 1F, 42F)
  val imNames = resourceNames
  val imDescriptions = resourceDescriptions
  val domainMetadata = Seq(Map("one" -> "1", "two" -> "3", "five" -> "8"), Map("one" -> "2"), Map("two" -> "3"), Map.empty[String,String])
  val isModerationApproveds = Seq(Some(false), Some(true), None, None, None)
  val isPublics = Seq(true)
  val isPublisheds = Seq(true)
  val isDefaultViews = Seq(false, false, false, true, false)
  val approvingDomains = Seq("petercetera.net", "blue.org", "annabelle.island.demo").map(Seq(_))
  val approvingDomainIds = Seq(Seq(0), Seq(2), Seq(3))
  val pageViewsTotal = Seq.range(1, Datatypes.materialized.length).map(_.toString)
  val domainCategories = Seq("Alpha to Omega", "Beta", "Gamma", "")
  val domainTags = Seq("1-one", "2-two", "3-three", "4-four").map(Seq(_))
  val updateFreqs = Seq(1, 2, 3, 4)
  val ownerIds = Seq("robin-hood", "lil-john")
  val ownerScreenNames = Seq("Robin Hood", "Little John")
  val attributions = Seq[Option[String]](None)
  val sharedTos = Seq[Option[Seq[String]]](None)

  def bootstrapData(): Unit = {
    ElasticsearchBootstrap.ensureIndex(client, "yyyyMMddHHmm", testSuiteName)

    // load domains
    domains.foreach { d =>
      client.client.prepareIndex(testSuiteName, esDomainType)
        .setSource(JsonUtil.renderJson[Domain](d))
        .setId(d.domainId.toString)
        .setRefresh(true)
        .execute.actionGet
    }

    // load users
    users.foreach { u =>
      client.client.prepareIndex(testSuiteName, esUserType)
        .setSource(JsonUtil.renderJson[EsUser](u))
        .setId(u.id)
        .setRefresh(true)
        .execute.actionGet
    }

    // load data
    Datatypes.materialized.zipWithIndex.foreach { case (datatype: Materialized, i: Int) =>
      client.client.prepareIndex(testSuiteName, esDocumentType)
        .setParent((i % domainsWithData.length).toString)
        .setSource(buildEsDocByIndex(i))
        .setRefresh(true)
        .execute.actionGet
    }
    // TODO: more refined test document creation
    Seq(
      (0, buildEsDoc(
        "zeta-0001", None, 0,
        "a stale moderation status", "zeta-0001", DateTime.now.toString, DateTime.now.toString,
        "zeta-0001", Seq.empty, "", None, Map.empty, Map.empty,
        TypeDatasets.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty,
        Map.empty,
        isPublic = true, isPublished = true, isDefaultView = false, Some(true), Seq(0), isApprovedByParentDomain = true,
        "42", "Fun", Seq.empty, 0L, "robin-hood", "Robin Hood", None, None
      )),
      (3, buildEsDoc(
        "zeta-0002", None, 3,
        "full routing & approval", "zeta-0002", DateTime.now.toString, DateTime.now.toString,
        "zeta-0002", Seq.empty, "", None, Map.empty, Map.empty,
        TypeDatasets.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty,
        Map.empty,
        isPublic = true, isPublished = true, isDefaultView = false, Some(true), Seq(2,3), isApprovedByParentDomain = true,
        "42", "Fun", Seq.empty, 0L, "lil-john", "Little John", None, None
      )),
      (0, buildEsDoc(
        "zeta-0003", None, 0,
        "private dataset", "zeta-0003", DateTime.now.toString, DateTime.now.toString,
        "zeta-0003", Seq.empty, "", None, Map.empty, Map.empty,
        TypeDatasets.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty,
        Map.empty,
        isPublic = false, isPublished = true, isDefaultView = true, Some(true), Seq(0, 1, 2,3), isApprovedByParentDomain = true,
        "42", "Private", Seq.empty, 0L, "robin-hood", "Robin Hood", None, None
      )),
      (0, buildEsDoc(
        "zeta-0004", None, 0,
        "standalone visualization", "zeta-0004", DateTime.now.toString, DateTime.now.toString,
        "zeta-0004", Seq.empty, "", None, Map.empty, Map.empty,
        TypeDatalensCharts.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty,
        Map.empty,
        isPublic = true, isPublished = true, isDefaultView = false, None, Seq(0, 1, 2,3), isApprovedByParentDomain = true,
        "42", "Standalone", Seq.empty, 0L, "lil-john", "Little John", None, Some(Seq("robin-hood"))
      )),
      (3, buildEsDoc(
        "zeta-0005", None, 3,
        "another john owned document", "zeta-0005", DateTime.now.toString, DateTime.now.toString,
        "zeta-0005", Seq.empty, "", None, Map.empty, Map.empty,
        TypeDatasets.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty, Map.empty,
        isPublic = true, isPublished = true, isDefaultView = true, Some(true), Seq(3), isApprovedByParentDomain = true,
        "42", "Fun", Seq.empty, 0L, "john-clan", "John McClane", None, None
      )),
      (0, buildEsDoc(
        "zeta-0006", None, 0,
        "unpublished dataset", "zeta-0006", DateTime.now.toString, DateTime.now.toString,
        "zeta-0006", Seq.empty, "", None, Map.empty, Map.empty,
        TypeDatasets.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty,
        Map.empty,
        isPublic = true, isPublished = false, isDefaultView = true, Some(true), Seq(0, 1, 2,3), isApprovedByParentDomain = true,
        "42", "Unpublished", Seq.empty, 0L, "robin-hood", "Robin Hood", None, None
       )),
      (0, buildEsDoc(
        "zeta-0007", None, 0,
        "a dataset with attribution", "zeta-0007", DateTime.now.toString, DateTime.now.toString,
        "zeta-0007", Seq.empty, "", Some("The Merry Men"), Map.empty, Map.empty,
        TypeDatasets.singular, viewtype = "", 0F,
        "", "", Seq.empty, Seq.empty, Seq.empty,
        Map.empty,
        isPublic = true, isPublished = true, isDefaultView = true, Some(true), Seq(0, 1, 2, 3), isApprovedByParentDomain = true,
        "42", "Fun", Seq.empty, 0L, "lil-john", "Little John",
        attribution = Some("The Merry Men"), None
      ))
    ).foreach { case (domain, doc) =>
      client.client.prepareIndex(testSuiteName, esDocumentType)
        .setParent(domain.toString)
        .setSource(doc)
        .setRefresh(true)
        .execute.actionGet
    }
  }

  def removeBootstrapData(): Unit = {
    client.client.admin().indices().prepareDelete(testSuiteName)
      .execute.actionGet
  }
}

