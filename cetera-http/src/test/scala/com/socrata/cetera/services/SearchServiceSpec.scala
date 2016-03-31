package com.socrata.cetera.services

import java.nio.charset.{Charset, CodingErrorAction}
import scala.collection.JavaConverters._

import com.rojoma.json.v3.ast.{JString, JValue}
import com.rojoma.json.v3.interpolation._
import com.rojoma.simplearm.v2.managed
import org.elasticsearch.action.search._
import org.elasticsearch.common.bytes.BytesArray
import org.elasticsearch.common.text.StringText
import org.elasticsearch.search.aggregations.{InternalAggregation, InternalAggregations}
import org.elasticsearch.search.facet.{Facet, InternalFacets}
import org.elasticsearch.search.internal._
import org.elasticsearch.search.suggest.Suggest
import org.elasticsearch.search.{SearchHitField, SearchShardTarget}
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import com.socrata.cetera._
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.search._
import com.socrata.cetera.types._
import com.socrata.cetera.util.Params

class SearchServiceSpec extends FunSuiteLike with Matchers with BeforeAndAfterAll {
  val testSuiteName: String = getClass.getSimpleName.toLowerCase
  val client: ElasticSearchClient = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val documentClient: DocumentClient = new DocumentClient(client, domainClient, testSuiteName, None, None, Set.empty)
  val balboaClient: BalboaClient = new BalboaClient("/tmp/metrics")
  val service: SearchService = new SearchService(documentClient, domainClient, balboaClient)

  override protected def afterAll(): Unit = {
    client.close() // Important!!
  }

  val emptySearchHitMap = Map[String,SearchHitField]().asJava

  val domainCnames = Map(
    0 -> "socrata.com",
    1 -> "first-socrata.com",
    2 -> "second-socrata.com")

  val searchResponse = {
    val shardTarget = new SearchShardTarget("1", "catalog", 1)
    val score = 0.12345f

    val resource = "\"resource\":{\"name\": \"Just A Test\", \"I'm\":\"OK\",\"you're\":\"so-so\"}"

    val datasetSocrataId =
      "\"socrata_id\":{\"domain_id\":[0],\"dataset_id\":\"four-four\"}"
    val pageSocrataId =
      "\"socrata_id\":{\"domain_id\":[1,2],\"dataset_id\":\"four-four\",\"page_id\":\"fore-fore\"}"

    val datasetDatatype = "\"datatype\":\"dataset\""
    val datasetViewtype = "\"viewtype\":\"\""
    val pageDatatype = "\"datatype\":\"datalens\""
    val pageViewtype = "\"viewtype\":\"\""

    val datasetSource = new BytesArray("{" + List(resource, datasetDatatype, datasetViewtype, datasetSocrataId).mkString(",") + "}")
    val pageSource = new BytesArray("{" + List(resource, pageDatatype, pageViewtype, pageSocrataId).mkString(",") + "}")

    val datasetHit = new InternalSearchHit(1, "46_3yu6-fka7", new StringText("dataset"), emptySearchHitMap)
    datasetHit.shardTarget(shardTarget)
    datasetHit.sourceRef(datasetSource)
    datasetHit.score(score)

    val updateFreq: SearchHitField = new InternalSearchHitField(
      "update_freq", List.empty[Object].asJava)

    val popularity: SearchHitField = new InternalSearchHitField(
      "popularity", List.empty[Object].asJava)

    val pageHit = new InternalSearchHit(1, "64_6uy3-7akf", new StringText("page"), emptySearchHitMap)
    pageHit.shardTarget(shardTarget)
    pageHit.sourceRef(pageSource)
    pageHit.score(score)

    val hits = Array[InternalSearchHit](datasetHit, pageHit)
    val internalSearchHits = new InternalSearchHits(hits, 3037, 1.0f)
    val internalSearchResponse = new InternalSearchResponse(
      internalSearchHits,
      new InternalFacets(List[Facet]().asJava),
      new InternalAggregations(List[InternalAggregation]().asJava),
      new Suggest(),
      false,
      false)

    new SearchResponse(internalSearchResponse, "", 15, 15, 4, Array[ShardSearchFailure]())
  }

  test("extract and format resources from SearchResponse") {
    val domain = Domain(
      isCustomerDomain = true,
      Some("Temp Org"),
      "tempuri.org",
      1,
      Some("Title"),
      moderationEnabled = false,
      routingApprovalEnabled = false,
      lockedDown = false,
      apiLockedDown = false)
    val resource = j"""{ "name" : "Just A Test", "I'm" : "OK", "you're" : "so-so" }"""

    val searchResults = service.format(domainCnames, showScore = false, searchResponse)

    searchResults.resultSetSize should be (None) // not yet added
    searchResults.timings should be (None) // not yet added

    val results = searchResults.results
    results should be ('nonEmpty)
    results.size should be (2)

    val datasetResponse = results(0)
    datasetResponse.resource should be (j"""${resource}""")
    datasetResponse.classification should be (Classification(Seq.empty[JValue], Seq.empty[JValue], None, None, None))

    datasetResponse.metadata.get(esDomainType) match {
      case Some(domain) => domain should be (JString("socrata.com"))
      case None => fail("metadata.domain field missing")
    }

    val pageResponse = results(1)
    pageResponse.resource should be (j"""${resource}""")
    pageResponse.classification should be (Classification(Seq.empty[JValue], Seq.empty[JValue], None, None, None))

    pageResponse.metadata.get(esDomainType) match {
      case Some(domain) => domain should be (JString("second-socrata.com"))
      case None => fail("metadata.domain field missing")
    }
  }

  test("build base urls and pretty seo urls") {
    val cname = "tempuri.org"
    val category = Some("Public Safety")
    val name = "Seattle Police Department 911 Incident Response"
    val id = "1234-abcd"

    val dt = "datatype"
    val vt = "viewtype"

    val xp = "expectedPermalink"
    val xpDefault = "/d/"

    val xs = "expectedSeolink"
    val xsDefault = "/Public-Safety/Seattle-Police-Department-911-Incident-Response/"

    Seq(
      Map(dt -> "calendar"),
      Map(dt -> "chart"),
      Map(dt -> "datalens", xp -> "/view/"),
      Map(dt -> "chart", vt -> "datalens", xp -> "/view/"),
      Map(dt -> "map", vt -> "datalens", xp -> "/view/"),
      Map(dt -> "dataset"),
      Map(dt -> "file"),
      Map(dt -> "filter"),
      Map(dt -> "form"),
      Map(dt -> "map", vt -> "geo"),
      Map(dt -> "map", vt -> "tabular"),
      Map(dt -> "href"),
      Map(dt -> "story", xp -> "/stories/s/", xs -> "/stories/s/")
    ).foreach { t =>
      val urls = SearchService.links(cname, Datatype(t.get(dt)), t.get(vt), id, category, name)
      urls.getOrElse("permalink", fail()).string should include(t.getOrElse(xp, xpDefault))
      urls.getOrElse("link", fail()).string should include(t.getOrElse(xs, xsDefault))
    }
  }

  test("pretty seo url - missing/blank category defaults to 'dataset'") {
    val cname = "tempuri.org"
    val id = "1234-asdf"
    val name = "this is a name"

    Seq(None, Some("")).foreach { category =>
      val urls = SearchService.links(cname, Option(TypeDatasets), None, id, category, name)
      urls.getOrElse("link", fail()).string should include("/dataset/this-is-a-name/1234-asdf")
    }
  }

  test("pretty seo url - missing/blank name defaults to '-'") {
    val cname = "tempuri.org"
    val id = "1234-asdf"
    val category = Some("this-is-a-category")

    Seq(null, "").foreach { name =>
      val urls = SearchService.links(cname, Option(TypeDatasets), None, id, category, name)
      urls.getOrElse("link", fail()).string should include("/this-is-a-category/-/1234-asdf")
    }
  }

  test("pretty seo url - limit 50 characters") {
    val cname = "tempuri.org"
    val id = "1234-asdf"
    val category = Some("A super long category name is not very likely but we will protect against it anyway")
    val name = "More commonly customers may write a title that is excessively verbose and it will hit this limit"
    val urls = SearchService.links(cname, Option(TypeDatasets), None, id, category, name)
    urls.getOrElse("link", fail()).string should include("/A-super-long-category-name-is-not-very-likely-but-/More-commonly-customers-may-write-a-title-that-is-/1234-asdf")
  }

  // NOTE: depending on your editor rendering, these RTL strings might look AWESOME(ly different)!
  // scalastyle:off non.ascii.character.disallowed
  test("pretty seo url - allows non-english unicode") {
    val cname = "tempuri.org"
    val id = "1234-asdf"
    val category = Some("بيانات عن الجدات")
    val name = "愛"
    val urls = SearchService.links(cname, Option(TypeDatasets), None, id, category, name)
    urls.getOrElse("link", fail()).string should include("بيانات-عن-الجدات")
    urls.getOrElse("link", fail()).string should include("愛")
  }

  ignore("es client - min should match") {}
  ignore("es client - slop") {}
  ignore("es client - function score") {}
  ignore("es client - advanced query") {}
  ignore("es client - score boosts") {}
  ignore("es client - domain metadata filter") {}
  ignore("es client - script score functions") {}
  ignore("es client - query with no filters, maybe?") {}
  ignore("es client - sort field asc/desc") {}
  ignore("popularity") {}
  ignore("update frequency") {}
  ignore("domain cname unexpected json value") {}
  ignore("query parameter parser - errors") {}
}

class SearchServiceSpecWithTestData extends FunSuiteLike with Matchers with TestESData with BeforeAndAfterAll {
  val client: ElasticSearchClient = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val documentClient: DocumentClient = new DocumentClient(client, domainClient, testSuiteName, None, None, Set.empty)
  val balboaDir: java.io.File = new java.io.File("balboa_test_trash")
  val balboaClient: BalboaClient = new BalboaClient(balboaDir.getName)
  val service: SearchService = new SearchService(documentClient, domainClient, balboaClient)

  def emptyAndRemoveDir(dir: java.io.File): Unit = {
    if (dir.isDirectory) {
      dir.listFiles().foreach(f => f.delete())
    }
    dir.delete()
  }

  override protected def beforeAll(): Unit = {
    bootstrapData()
    emptyAndRemoveDir(balboaDir)
  }

  override protected def afterAll(): Unit = {
    removeBootstrapData()
    client.close()
    emptyAndRemoveDir(balboaDir)
  }

  def wasSearchQueryLogged(filename: String, q: String): Boolean = {
    val decoder = Charset.forName("UTF-8").newDecoder()
    decoder.onMalformedInput(CodingErrorAction.IGNORE)
    for (s <- managed(scala.io.Source.fromFile(filename)(decoder))) {
      val entries = s.getLines.toList(0)
      entries.contains(s"datasets-search-$q")
    }
  }

  test("search response contains pretty and perma links") {
    service.doSearch(Map.empty, None)._1.results.foreach { r =>
      val dsid = r.resource.dyn.id.!.asInstanceOf[JString].string

      val perma = "(d|stories/s|view)"
      val alphanum = "[\\p{L}\\p{N}\\-]+" // all of the test data have proper categories and names
      val pretty = s"$alphanum/$alphanum"

      r.permalink.string should endWith regex s"/$perma/$dsid"
      r.link.string should endWith regex s"/$pretty/$dsid"
    }
  }

  test("if a domain is given in the searchContext and a simple query is given, the query should be logged") {
    val query = "log this query"
    val params = Map(
      "domains" -> "opendata-demo.socrata.com,petercetera.net",
      "search_context" -> "opendata-demo.socrata.com",
      "q" -> query
    ).mapValues(Seq(_))
    service.doSearch(params, None)._1.results
    val metricsFile = balboaDir.listFiles()(0)
    wasSearchQueryLogged(metricsFile.getAbsolutePath, query) should be(true)
  }

  test("if a domain is given in the searchContext and an advance query is given, the query should be logged") {
    val query = "(log this query OR don't) AND (check up on it OR don't)"
    val params = Map(
      "domains" -> "opendata-demo.socrata.com,petercetera.net",
      "search_context" -> "opendata-demo.socrata.com",
      "q_internal" -> query
    ).mapValues(Seq(_))
    service.doSearch(params, None)._1.results
    val metricsFile = balboaDir.listFiles()(0)
    wasSearchQueryLogged(metricsFile.getAbsolutePath, query) should be(true)
  }

  test("if there is no searchContext, no query should be logged") {
    val query = "don't log this query"
    val params = Map(
      "domains" -> "opendata-demo.socrata.com,petercetera.net",
      "q" -> query
    ).mapValues(Seq(_))
    service.doSearch(params, None)._1.results
    val metricsFile = balboaDir.listFiles()(0)
    wasSearchQueryLogged(metricsFile.getAbsolutePath, query) should be(false)
  }

  test("search response without a searchContext should have the correct set of documents") {
    // TODO: as we add more and more domain filters, the approach to
    // creating data in TestESData will need to change so the test
    // data is more comprehensive and running these sorts of tests
    // is easier
    /*
      Test Data domains -> documents
      id  cname                     cust  mod   r&a
      0   petercetera.net           t     f     f (not moderated, 1 default view)
        documents
        fxf     def mod r&a       visible
        fxf-0   f   f   0         t
        fxf-4   f   n   2         t
        fxf-8   t   n   3         t
        zeta-1  f   t   0         t       (mod=t anomaly)
        zeta-3  t   t   0,1,2,3   f       (isPublic=false)
        zeta-4  f   n   0,1,2,3   f       (unapproved datalens_chart/map should not be visible)
      1   opendata-demo.socrata.com f     t     f (not customer domain)
        fxf     def mod r&a visible
        fxf-1   f   t   2   f / t
        fxf-5   f   f   3   f / f
        fxf-9   f   n   0   f / f
      2   blue.org                  t     f     t (not moderated, 0 default views)
        fxf     def mod r&a visible
        fxf-2   f   n   3   f
        fxf-6   f   t   0   f
        fxf-10  f   f   2   t
      3   annabelle.island.net      t     t     t
        fxf     def mod r&a visible
        fxf-3   t   n   0   f
        fxf-7   f   n   2   f
        zeta-2  f   t   2,3 t
     */
    val expectedFxfs = Set("fxf-0", "fxf-4", "fxf-8", "fxf-10", "zeta-0001", "zeta-0002")
    // this shows that:
    //   * rejected and pending views don't show up regardless of domain setting
    //   * that the ES type returned includes only documents (i.e. no domains)
    //   * that non-customer domains don't show up
    val (res, _) = service.doSearch(Map.empty, None)
    val actualFxfs = res.results.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

  test("private documents should always be hidden") {
    val expectedFxfs = Set.empty
    val (res, _) = service.doSearch(Map(
      Params.filterDomains -> "petercetera.net",
      Params.context -> "petercetera.net",
      Params.querySimple -> "private"
    ).mapValues(Seq(_)), None)
    val actualFxfs = res.results.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

  test("not_moderated data federated to a moderated domain should not be in the response") {
    // the domain params will limit us to fxfs 0,4,8 and 1,5,9
    val params = Map(
      "domains" -> "opendata-demo.socrata.com,petercetera.net",
      "search_context" -> "opendata-demo.socrata.com"
    ).mapValues(Seq(_))
    // of those fxfs, only show: fxf-1 is approved and fxf-8 is a default view
    val expectedFxfs = Set("fxf-1", "fxf-8")
    val res = service.doSearch(params, None)._1.results
    val actualFxfs = res.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

  test("if a domain has routing & approval, only parent domain approved datasets should show up") {
    // the domain params will limit us to fxfs 2,6,10
    val params = Map(
      "domains" -> "blue.org"
    ).mapValues(Seq(_))
    // only these fxfs are approved by parent domain:
    val expectedFxfs = Set("fxf-10")
    val res = service.doSearch(params, None)._1.results
    val actualFxfs = res.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

  test("if a search context has routing & approval, only datasets approved by that domain too should show up") {
    val params = Map(
      "domains" -> "blue.org,annabelle.island.net",
      "search_context" -> "blue.org"
    ).mapValues(Seq(_))
    // only these fxfs are approved by search context AND parent domain:
    val expectedFxfs = Set("fxf-10", "zeta-0002")
    val res = service.doSearch(params, None)._1.results
    val actualFxfs = res.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

  test("categories filter should be case insensitive") {
    val paramsTitleCase = Map(
      "domains" -> Seq("petercetera.net"),
      "categories" -> Seq("Personal")
    )
    val paramsLowerCase = paramsTitleCase.mapValues(_.map(_.toLowerCase))
    val paramsUpperCase = paramsTitleCase.mapValues(_.map(_.toUpperCase))

    val (resultsTitleCase, _) = service.doSearch(paramsTitleCase, None)
    val (resultsLowerCase, _) = service.doSearch(paramsLowerCase, None)
    val (resultsUpperCase, _) = service.doSearch(paramsUpperCase, None)

    resultsTitleCase.results should contain theSameElementsAs resultsLowerCase.results
    resultsTitleCase.results should contain theSameElementsAs resultsUpperCase.results
  }

  test("custom domain categories filter should be case insensitive") {
    val paramsTitleCase = Map(
      "domains" -> Seq("petercetera.net"),
      "search_context" -> Seq("petercetera.net"),
      "categories" -> Seq("Alpha")
    )
    val paramsLowerCase = paramsTitleCase.mapValues(_.map(_.toLowerCase))
    val paramsUpperCase = paramsTitleCase.mapValues(_.map(_.toUpperCase))

    val (resultsTitleCase, _) = service.doSearch(paramsTitleCase, None)
    val (resultsLowerCase, _) = service.doSearch(paramsLowerCase, None)
    val (resultsUpperCase, _) = service.doSearch(paramsUpperCase, None)

    resultsTitleCase.results should contain theSameElementsAs resultsLowerCase.results
    resultsTitleCase.results should contain theSameElementsAs resultsUpperCase.results
  }

  test("tags filter should be case insensitive") {
    val paramsTitleCase = Map(
      "domains" -> Seq("petercetera.net"),
      "tags" -> Seq("Happy")
    )
    val paramsLowerCase = paramsTitleCase.mapValues(_.map(_.toLowerCase))
    val paramsUpperCase = paramsTitleCase.mapValues(_.map(_.toUpperCase))

    val (resultsTitleCase, _) = service.doSearch(paramsTitleCase, None)
    val (resultsLowerCase, _) = service.doSearch(paramsLowerCase, None)
    val (resultsUpperCase, _) = service.doSearch(paramsUpperCase, None)

    resultsTitleCase.results should contain theSameElementsAs resultsLowerCase.results
    resultsTitleCase.results should contain theSameElementsAs resultsUpperCase.results
  }

  test("custom domain tags filter should be case insensitive") {
    val paramsTitleCase = Map(
      "domains" -> Seq("petercetera.net"),
      "search_context" -> Seq("petercetera.net"),
      "tags" -> Seq("1-One")
    )
    val paramsLowerCase = paramsTitleCase.mapValues(_.map(_.toLowerCase))
    val paramsUpperCase = paramsTitleCase.mapValues(_.map(_.toUpperCase))

    val (resultsTitleCase, _) = service.doSearch(paramsTitleCase, None)
    val (resultsLowerCase, _) = service.doSearch(paramsLowerCase, None)
    val (resultsUpperCase, _) = service.doSearch(paramsUpperCase, None)

    resultsTitleCase.results should contain theSameElementsAs resultsLowerCase.results
    resultsTitleCase.results should contain theSameElementsAs resultsUpperCase.results
  }

  test("sorting by name works") {
    val params = Map("order" -> Seq("name"))
    val (results, _) = service.doSearch(params, None)
    val expected = results.results.map(_.resource.dyn("name").!.asInstanceOf[JString].string).sorted.head
    val firstResult = results.results.head.resource.dyn("name").? match {
      case Right(n) => n should be (JString(expected))
      case Left(_) => fail("resource had no name!")
    }
  }

  test("sorting by name DESC works") {
    val params = Map("order" -> Seq("name DESC"))
    val (results, _) = service.doSearch(params, None)
    val expected = results.results.map(_.resource.dyn("name").!.asInstanceOf[JString].string).sorted.last
    val firstResult = results.results.head.resource.dyn("name").? match {
      case Right(n) => n should be (JString(expected))
      case Left(_) => fail("resource had no name!")
    }
  }
}
