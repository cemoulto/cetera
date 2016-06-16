package com.socrata.cetera.util

import org.scalatest.{FunSuiteLike, Matchers}

import com.socrata.cetera.types._

class QueryParametersParserSpec extends FunSuiteLike with Matchers {
  val qpp = QueryParametersParser

  test("validate a Right(Int)") {
    qpp.validated(Right(100)) should be (100)
  }

  test("'only' query parameter allows blank to indicate no filter") {
    qpp.restrictParamFilterDatatype(None) should be(Right(None))
    qpp.restrictParamFilterDatatype(Some("")) should be(Right(None))
  }

  test("'only' query parameter allows all datatypes") {
    Datatypes.all.foreach { datatype =>
      qpp.restrictParamFilterDatatype(Some(datatype.plural)) should be(Right(Some(datatype.names)))
      qpp.restrictParamFilterDatatype(Some(datatype.singular)) should be(Right(Some(datatype.names)))
    }
  }

  test("'only' query parameter specific instances") {
    qpp.restrictParamFilterDatatype(Some("calendars")) should be(Right(Some(Set("calendar"))))
    qpp.restrictParamFilterDatatype(Some("datalenses")) should be(Right(Some(Set("datalens"))))
    qpp.restrictParamFilterDatatype(Some("datasets")) should be(Right(Some(Set("dataset"))))
    qpp.restrictParamFilterDatatype(Some("files")) should be(Right(Some(Set("file"))))
    qpp.restrictParamFilterDatatype(Some("filters")) should be(Right(Some(Set("filter"))))
    qpp.restrictParamFilterDatatype(Some("forms")) should be(Right(Some(Set("form"))))
    qpp.restrictParamFilterDatatype(Some("stories")) should be(Right(Some(Set("story"))))

    val linksExpected = Set("href")
    qpp.restrictParamFilterDatatype(Some("links")) should be(Right(Some(linksExpected)))

    val chartsExpected = Set("chart", "datalens_chart")
    qpp.restrictParamFilterDatatype(Some("charts")) should be(Right(Some(chartsExpected)))

    val mapsExpected = Set("datalens_map", "geo_map", "map", "tabular_map")
    qpp.restrictParamFilterDatatype(Some("maps")) should be(Right(Some(mapsExpected)))
  }

  test("datatype boost parameter parses with valid datatype") {
    Params.datatypeBoostParam("boostDatasets") should contain(TypeDatasets)
  }

  test("datatype boost parameter ignore with invalid datatype") {
    Params.datatypeBoostParam("boostFoo") shouldBe empty
  }

  test("datatype boost parameter parsing is case-sensitive") {
    Params.datatypeBoostParam("BOOSTDATASETS") shouldBe empty
    Params.datatypeBoostParam("boostdatasets") shouldBe empty
    Params.datatypeBoostParam("boostDatasets") should contain(TypeDatasets)
  }

  test("field boosts are treated separately from datatype boosts") {
    val expectedFieldBoosts = Map(
        ColumnNameFieldType -> 10.0,
        ColumnDescriptionFieldType -> 10.0,
        ColumnFieldNameFieldType -> 10.0,

        TitleFieldType -> 9.0,
        DescriptionFieldType -> 8.0)

    val vqps = qpp(
      Map(
        "boostColumns" -> "10.0", // should produce 3 boosts: name, description, field_name

        "boostTitle" -> "9.0",
        "boostDesc" -> "8.0"
      ).mapValues(Seq(_)), None)

    vqps match {
      case Left(_) => fail("a ValidatedQueryParameters should be returned")
      case Right(params) =>
        params.fieldBoosts should be (expectedFieldBoosts)
        params.datatypeBoosts should be (Map())
    }
  }

  test("datatype boosts are not treated as custom metadata key-value pairs") {
    val input = Map("boostCharts" -> "10.0", "foo" -> "bar").mapValues(Seq(_))
    val expected = Map("foo" -> "bar").mapValues(Seq(_))
    Params.remaining(input) should be(expected)
  }

  test("'only' query parameter allows multiple selections") {
    QueryParametersParser(Map("only" -> Seq("datasets,datalenses")), None) match {
      case Left(_) => fail("a ValidatedQueryParameters should be returned")
      case Right(params) =>
        params.datatypes should be('defined)
        params.datatypes.get should be(Set(TypeDatasets.singular, TypeDatalenses.singular))
    }
  }

  test("'only[]' query parameter allows multiple selections") {
    QueryParametersParser(Map("only[]" -> Seq("datasets", "datalenses")), None) match {
      case Left(_) => fail("a ValidatedQueryParameters should be returned")
      case Right(params) =>
        params.datatypes should be('defined)
        params.datatypes.get should be(Set(TypeDatasets.singular, TypeDatalenses.singular))
    }
  }

  test("no datatype boost params results in empty datatype boosts map") {
    QueryParametersParser(Map("query" -> "crime").mapValues(Seq(_)), None) match {
      case Right(params) => params.datatypeBoosts should have size 0
      case _ => fail()
    }
  }

  test("malformed datatype boost params result in empty datatype boosts map") {
    QueryParametersParser(Map("query" -> "crime", "boostsDatasets" -> "5.0").mapValues(Seq(_)), None) match {
      case Right(params) => params.datatypeBoosts should have size 0
      case _ => fail()
    }
  }

  test("well-formed datatype boost params validate") {
    QueryParametersParser(Map("query" -> "crime", "boostDatasets" -> "5.0", "boostMaps" -> "2.0").mapValues(Seq(_)), None) match {
      case Right(params) => params.datatypeBoosts should have size 2
      case _ => fail()
    }
  }

  test("allow category with commas") {
    QueryParametersParser(Map("categories" -> "Traffic, Parking, and Transportation").mapValues(Seq(_)), None) match {
      case Right(params) =>
        params.categories should be('defined)
        params.categories.get should have size 1
        params.categories.get.head should be("Traffic, Parking, and Transportation")
      case _ => fail()
    }
  }

  test("allow tag with commas") {
    QueryParametersParser(Map("tags" -> "this, probably, doesn't, happen, on, any, customer, sites").mapValues(Seq(_)), None) match {
      case Right(params) =>
        params.tags should be('defined)
        params.tags.get should have size 1
        params.tags.get.head should be("this, probably, doesn't, happen, on, any, customer, sites")
      case _ => fail()
    }
  }

  test("allow multiple category parameters") {
    QueryParametersParser(Map("categories" -> Seq("Traffic", "Parking", "Transportation")), None) match {
      case Right(params) =>
        params.categories should be('defined)
        params.categories.get should have size 3
        params.categories.get should contain theSameElementsAs Seq("Traffic", "Parking", "Transportation")
      case _ => fail()
    }
  }

  test("allow multiple tag parameters") {
    QueryParametersParser(Map("tags" -> Seq("Traffic", "Parking", "Transportation")), None) match {
      case Right(params) =>
        params.tags should be('defined)
        params.tags.get should have size 3
        params.tags.get should contain theSameElementsAs Seq("Traffic", "Parking", "Transportation")
      case _ => fail()
    }
  }

  test("also allow categories[] parameters") {
    QueryParametersParser(Map("categories" -> Seq("foo", "foos"), "categories[]" -> Seq("bar", "baz")), None) match {
      case Right(params) =>
        params.categories should be('defined)
        params.categories.get should have size 4
        params.categories.get should contain theSameElementsAs Seq("foo", "foos", "bar", "baz")
      case _ => fail()
    }
  }

  test("also allow tags[] parameters") {
    QueryParametersParser(Map("tags" -> Seq("foo", "foos"), "tags[]" -> Seq("bar", "baz")), None) match {
      case Right(params) =>
        params.tags should be('defined)
        params.tags.get should have size 4
        params.tags.get should contain theSameElementsAs Seq("foo", "foos", "bar", "baz")
      case _ => fail()
    }
  }

  test("domain metadata excludes known parameters") {
    val knownEnumParams = Map(
      "only" -> Seq(
        "calendar",
        "chart",
        "datalens",
        "dataset",
        "file",
        "filter",
        "form",
        "map",
        "href",
        "pulse",
        "story",
        "link",
        "datalens_chart",
        "datalens_map",
        "tabular_map"
      ),

      "order" -> Seq(
        "relevance",
        "page_views_last_month",
        "page_views_last_week",
        "page_views_total",
        "created_at",
        "updated_at"
      )
    )

    val knownNumericParams = List(
      "boostColumns",
      "boostDesc",
      "boostTitle",
      "limit",
      "offset",
      "slop",
      "boostCalendars",
      "boostCharts",
      "boostDatalenses",
      "boostDatasets",
      "boostFiles",
      "boostFilters",
      "boostForms",
      "boostMaps",
      "boostHrefs",
      "boostPulses",
      "boostStories",
      "boostLinks",
      "boostDatalens_charts",
      "boostDatalens_maps",
      "boostTabular_maps"
    ).map(p => p -> Seq("42")).toMap

    val knownStringParams = List(
      "search_context",
      "domains",
      "categories",
      "tags",
      "q",
      "q_internal",
      "min_should_match",
      "show_feature_vals",
      "show_score",
      "function_score"
    ).map(p => p -> Seq(p)).toMap

    val knownArrayParams = List(
      "categories[]",
      "tags[]"
    ).map(p => p -> Seq(p)).toMap

    // This is how they show up from socrata-http
    val domainBoostExamples = List(
      "boostDomains[example.com]",
      "boostDomains[data.seattle.gov]",
      "boostDomains[xyz]",
      "boostDomains[]"
    ).map(p => p -> Seq("1.23"))

    QueryParametersParser(
      knownEnumParams ++
        knownNumericParams ++
        knownStringParams ++
        knownArrayParams ++
        domainBoostExamples,
      None
    ) match {
      case Right(params) =>
        params.domainMetadata shouldNot be('defined)
      case _ => fail()
    }
  }

  // just documenting that we do not support boostDomains without the []
  test ("boostDomains without [] gets interpreted as custom metadata") {
    QueryParametersParser(Map("boostDomains" -> Seq("1.23"), "pants" -> Seq("2.34")), None) match {
      case Right(params) => params.domainMetadata match {
        case Some(metadata) => metadata should be(Set("boostDomains" -> "1.23", "pants" -> "2.34"))
        case None => fail("expected to see boostDomains show up in metadata")
      }
      case Left(e) => fail(e.toString)
    }
  }

  // empty query string param is passed in from socrata-http multi params sometimes, e.g. catalog?q=bikes&
  test("handle empty query string param key") {
    QueryParametersParser(Map("" -> Seq()), None) match {
      case Right(params) => ()
      case _ => fail()
    }
  }

  // empty query string param is passed in from socrata-http multi params sometimes, e.g. catalog?q=bikes&one+extra
  test("handle empty query string param value") {
    QueryParametersParser(Map("one extra" -> Seq()), None) match {
      case Right(params) => ()
      case _ => fail()
    }
  }

  test("handle empty string query param value") {
    QueryParametersParser(Map("q" -> Seq("")), None) match {
      case Right(params) => params.domains shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("q_internal" -> Seq("")), None) match {
      case Right(params) => params.domains shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("domains" -> Seq("")), None) match {
      case Right(params) => params.domains shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("search_context" -> Seq("")), None) match {
      case Right(params) => params.searchContext shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("categories" -> Seq("")), None) match {
      case Right(params) => params.categories shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("tags" -> Seq("")), None) match {
      case Right(params) => params.tags shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("datatypes" -> Seq("")), None) match {
      case Right(params) => params.datatypes shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("for_user" -> Seq("")), None) match {
      case Right(params) => params.user shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("attribution" -> Seq("")), None) match {
      case Right(params) => params.user shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("derived_from" -> Seq("")), None) match {
      case Right(params) => params.parentDatasetId shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("recipient" -> Seq("")), None) match {
      case Right(params) => params.parentDatasetId shouldNot be('defined)
      case _ => fail()
    }

    QueryParametersParser(Map("custom_metadata" -> Seq("")), None) match {
      case Right(params) => params.parentDatasetId shouldNot be('defined)
      case _ => fail()
    }
  }

  test("domain boosts can be parsed") {
    val domainBoosts = Map(
      "boostDomains[example.com]" -> Seq("1.23"),
      "boostDomains[data.seattle.gov]" -> Seq("4.56")
    )

    QueryParametersParser(domainBoosts, None) match {
      case Right(params) => params.domainBoosts should be(
        Map("example.com" -> 1.23f, "data.seattle.gov" -> 4.56f)
      )
      case _ => fail()
    }
  }

  test("domain boost defined twice will be completely ignored -- just documenting behavior") {
    val domainBoosts = Map(
      "boostDomains[example.com]" -> Seq("1.23", "2.34"),
      "boostDomains[data.seattle.gov]" -> Seq("4.56")
    )

    QueryParametersParser(domainBoosts, None) match {
      case Right(params) => params.domainBoosts should be(Map("data.seattle.gov" -> 4.56f))
      case _ => fail()
    }
  }

  test("domain boosts missing fields do not explode the params parser") {
    val domainBoosts = Map(
      "boostDomains[data.seattle.gov]" -> Seq("4.56"),
      "boostDomains[example.com]" -> Seq(),
      "boostDomains[]" -> Seq("7.89"),
      "boostDomains[]" -> Seq()
    )

    QueryParametersParser(domainBoosts, None) match {
      case Right(params) => params.domainBoosts should be(Map("data.seattle.gov" -> 4.56f))
      case _ => fail()
    }
  }

  test("domain boost degenerate cases do not explode the params parser") {
    val domainBoosts = Map(
      "boostDomains[boostDomains[example.com]]" -> Seq("1.23")
    )

    QueryParametersParser(domainBoosts, None) match {
      case Right(params) => params.domainBoosts should be(Map("boostDomains[example.com]" -> 1.23f))
      case _ => fail()
    }
  }

  test("domain boost params are not interpreted as custom metadata fields") {
    val params = Map(
      Params.context -> Seq("example.com"),
      Params.boostDomains + "[example.com]" -> Seq("1.23")
    )

    QueryParametersParser(params, None) match {
      case Right(p) => p.domainBoosts should be(Map("example.com" -> 1.23f))
      case _ => fail()
    }
  }

  // just documenting current if not-quite-ideal behavior
  test("boostDomains without [] is not supported and gets interpreted as metadata") {
    val params = Map(
      Params.context -> Seq("example.com"),
      Params.boostDomains + "example.com" -> Seq("1.23"),
      Params.boostDomains -> Seq("1.23")
    )

    QueryParametersParser(params, None) match {
      case Right(p) => p.domainBoosts should be(Map.empty[String, Float])
      case _ => fail()
    }
  }

  test("sort order can be parsed") {
    val sortOrder = Map("order" -> Seq("page_views_total"))

    QueryParametersParser(sortOrder, None) match {
      case Right(params) => params.sortOrder should be(Some("page_views_total"))
      case _ => fail()
    }
  }

  test("an upper bound of 10000 is imposed on the limit parameter") {
    val limit = Map("limit" -> Seq("100000"))

    QueryParametersParser(limit, None) match {
      case Right(params) => params.limit should be(10000)
      case _ => fail()
    }
  }

  test("a limit param of less than 10000 is allowed") {
    val limit = Map("limit" -> Seq("100"))

    QueryParametersParser(limit, None) match {
      case Right(params) => params.limit should be(100)
      case _ => fail()
    }
  }
}

class ParamsSpec extends FunSuiteLike with Matchers {
  test("isCatalogKey can recognize string keys") {
    val keys = Seq("search_context", "only", "domains", "slop", "boostMaps")
    keys.foreach { key =>
      Params.isCatalogKey(key) should be (true)
      Params.isCatalogKey(key.reverse) should be (false)
     }
  }

  test("isCatalogKey can recognize array keys") {
    val keys = Seq("categories[]", "tags[]")
    keys.foreach { key =>
      Params.isCatalogKey(key) should be (true)
      Params.isCatalogKey("phony_" + key) should be (false)
    }
  }

  test("isCatalogKey can recognize hashmap keys like boostDomains[]") {
    val keys = Seq("boostDomains[example.com]", "boostDomains[data.seattle.gov]")
    keys.foreach { key =>
      Params.isCatalogKey(key) should be (true)
      Params.isCatalogKey("phony_" + key) should be (false)
    }
  }

  test("isCatalogKey does not recognize boostDomains without []") {
    Params.isCatalogKey("boostDomains") should be (false)
  }

  test("isCatalogKey recognized sort order") {
    Params.isCatalogKey("order") should be (true)
  }

  test("isCatalogKey recognizes attribution") {
    Params.isCatalogKey("attribution") should be (true)
  }
}
