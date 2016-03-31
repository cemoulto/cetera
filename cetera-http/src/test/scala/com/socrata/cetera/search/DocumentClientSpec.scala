package com.socrata.cetera.search

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.interpolation._
import com.rojoma.json.v3.io.JsonReader
import org.elasticsearch.action.search.SearchType.COUNT
import org.scalatest.{BeforeAndAfterAll, ShouldMatchers, WordSpec}

import com.socrata.cetera.types._
import com.socrata.cetera.util.{ElasticsearchBootstrap, ValidatedQueryParameters}
import com.socrata.cetera.{TestESClient, esDocumentType}

///////////////////////////////////////////////////////////////////////////////
// NOTE: Regarding Brittleness
//
// These tests are very brittle because they test (typically) the JSON blob
// that gets sent to ES, and JSON blobs make no guarantees about order.
//
// These tests serve to codify expected behavior of our query builders.
// (e.g, given the following user input, did we build the correct sort order?)
// They do not test the correctness of those queries.
//
// Also note that a query builder is different from a request builder.

class DocumentClientSpec extends WordSpec with ShouldMatchers with BeforeAndAfterAll {
  val testSuiteName = getClass.getSimpleName.toLowerCase
  val client = new TestESClient(testSuiteName)  // Remember to close() me!!
  val defaultMinShouldMatch = Some("37%")
  val scriptScoreFunctions = Set(
    ScriptScoreFunction.getScriptFunction("views"),
    ScriptScoreFunction.getScriptFunction("score")
  ).flatMap { fn => fn }

  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val documentClient: DocumentClient = new DocumentClient(
    esClient = client,
    domainClient,
    indexAliasName = testSuiteName,
    defaultTitleBoost = None,
    defaultMinShouldMatch = defaultMinShouldMatch,
    scriptScoreFunctions = scriptScoreFunctions
  )

  override protected def beforeAll(): Unit = {
    ElasticsearchBootstrap.ensureIndex(client, "yyyyMMddHHmm", testSuiteName)
  }

  override protected def afterAll(): Unit = {
    client.close() // Important!!
  }

  val domainIds = Set(1, 2, 3)
  val params = ValidatedQueryParameters(
    searchQuery = SimpleQuery("search query terms"),
    domains = Set("www.example.com", "test.example.com", "socrata.com"),
    domainMetadata = None,
    searchContext = None,
    categories = Some(Set("Social Services", "Environment", "Housing & Development")),
    tags = Some(Set("taxi", "art", "clowns")),
    only = Some(Seq("datasets")),
    fieldBoosts = Map[CeteraFieldType with Boostable, Float](
      TitleFieldType -> 2.2f,
      DescriptionFieldType -> 1.1f
    ),
    datatypeBoosts = Map.empty,
    domainBoosts = Map.empty[String, Float],
    minShouldMatch = None,
    slop = None,
    showScore = false,
    offset = 10,
    limit = 20,
    sortOrder = Option("relevance") // should be the same as None
  )

  val shouldMatch = j"""{
    "multi_match": {
        "fields": [
            "fts_analyzed",
            "fts_raw"
        ],
        "query": "search query terms",
        "type": "phrase"
    }
  }"""

  val boostedShouldMatch = j"""{
    "multi_match": {
        "fields": [
            "fts_analyzed",
            "fts_raw",
             "indexed_metadata.name^2.2",
             "indexed_metadata.description^1.1"
        ],
        "query": "search query terms",
        "type": "phrase"
    }
  }"""

  val boolQuery = j"""{
    "bool": {
        "must": {
            "multi_match": {
                "fields": [
                    "fts_analyzed",
                    "fts_raw"
                ],
                "query": "search query terms",
                "type": "cross_fields"
            }
        },
        "should": ${shouldMatch}
    }
  }"""

  val boostedBoolQuery = j"""{
    "bool": {
        "must": {
            "multi_match": {
                "fields": [
                    "fts_analyzed",
                    "fts_raw"
                ],
                "query": "search query terms",
                "type": "cross_fields"
            }
        },
        "should": ${boostedShouldMatch}
    }
  }"""

  val moderationFilter = j"""{
    "bool": {
      "should": [
        {"term": {"is_default_view": true}},
        {"term": {"is_moderation_approved": true}}
      ]
    }
  }"""

  val routingApprovalFilter = j"""{
    "bool": {
      "must": {
        "bool": {
          "should": { "term": {"is_approved_by_parent_domain": true} }
        }
      }
    }
  }"""

  val domainFilter = j"""{
    "terms": {
      "socrata_id.domain_id": [
        1,
        2,
        3
      ]
    }
  }"""

  val publicFilter = j"""{
    "not": {
      "filter": {
        "term": { "is_public": false }
      }
    }
  }"""

  val defaultFilter = j"""{
    "bool": {
      "must": [
        ${publicFilter},
        ${moderationFilter},
        ${routingApprovalFilter}
      ]
    }
  }"""

  val defaultFilterPlusDomainIds = j"""{
    "bool": {
      "must": [
        ${publicFilter},
        ${domainFilter},
        ${moderationFilter},
        ${routingApprovalFilter}
      ]
    }
  }"""

  val datatypeDatasetsFilter = j"""{
    "terms": {
        "datatype": [
            "dataset"
        ]
    }
  }"""

  val animlCategoriesQuery = j"""{
    "nested": {
      "query": {
        "bool": {
          "should": [
            { "match": { "animl_annotations.categories.name" : { "query":"Social Services", "type":"boolean"} } },
            { "match": { "animl_annotations.categories.name" : { "query":"Environment", "type":"boolean"} } },
            { "match": { "animl_annotations.categories.name" : { "query":"Housing & Development", "type":"boolean"} } }
          ],
          "minimum_should_match" : "1"
        }
      },
      "path": "animl_annotations.categories"
    }
  }"""

  val animlTagsQuery = j"""{
    "nested": {
      "query": {
        "bool": {
          "should": [
            { "match": { "animl_annotations.tags.name" : { "query":"taxi", "type":"boolean" } } },
            { "match": { "animl_annotations.tags.name" : { "query":"art", "type":"boolean" } } },
            { "match": { "animl_annotations.tags.name" : { "query":"clowns", "type":"boolean" } } }
          ],
          "minimum_should_match" : "1"
        }
      },
      "path": "animl_annotations.tags"
    }
  }"""

  val simpleQuery = j"""{
    "bool": {
      "must": [
        {"match_all": {}},
        ${animlCategoriesQuery},
        ${animlTagsQuery},
        ${domainFilter}
      ]
    }
  }"""

  val simpleQueryWithoutDomainFilter = j"""{
    "bool": {
      "must": [
        {"match_all": {}},
        ${animlCategoriesQuery},
        ${animlTagsQuery}
      ]
    }
  }"""

  val complexQuery = j"""{
    "bool": {
      "must": [
        ${boolQuery},
        ${animlCategoriesQuery},
        ${animlTagsQuery}
      ]
    }
  }"""

  val boostedComplexQuery = j"""{
    "bool": {
      "must": [
        ${boostedBoolQuery},
        ${animlCategoriesQuery},
        ${animlTagsQuery}
      ]
    }
  }"""

  val complexFilter = j"""{
    "bool": {
        "must": [
            ${datatypeDatasetsFilter},
            ${publicFilter},
            ${moderationFilter},
            ${routingApprovalFilter}
        ]
    }
  }"""

  val matchAll = j"""{ "match_all" : {} }"""

  def functionScoreQuery(query: JValue) = j"""{
    "function_score" : {
      "query" : ${query},
      "functions" : [
        {
          "script_score" : {
            "script" : "1 + doc[\"page_views.page_views_total_log\"].value",
            "lang" : "expression"
          }
        },
        {
          "script_score" :
          { "script" : "_score", "lang" : "expression" }
        }
      ],
      "score_mode" : "multiply",
      "boost_mode" : "replace"
    }
  }"""

  ///////////////////
  // buildBaseRequest
  ///////////////////

  "buildBaseRequest" should {
    "construct a default match all query" in {
      val query = j"""{
        "filtered": {
          "filter": ${defaultFilter},
          "query": ${matchAll}
        }
      }"""

      val expected = j"""{
        "query": ${functionScoreQuery(query)}
      }"""

      val request = documentClient.buildBaseRequest(
        searchQuery = NoQuery,
        domains = Set.empty,
        searchContext = None,
        categories = None,
        tags = None,
        domainMetadata = None,
        only = None,
        fieldBoosts = Map.empty,
        datatypeBoosts = Map.empty,
        domainIdBoosts = Map.empty[Int, Float],
        minShouldMatch = None,
        slop = None
      )
      val actual = JsonReader.fromString(request.toString)

      actual should be (expected)
      request.request.types should be (Array(esDocumentType))
    }

    "construct a match query with terms" in {
      val query = j"""{
        "filtered": {
          "filter": ${defaultFilter},
          "query": ${boolQuery}
        }
      }"""

      val expected = j"""{
        "query": ${functionScoreQuery(query)}
      }"""

      val request = documentClient.buildBaseRequest(
        searchQuery = params.searchQuery,
        domains = Set.empty,
        searchContext = None,
        categories = None,
        tags = None,
        domainMetadata = None,
        only = None,
        fieldBoosts = Map.empty,
        datatypeBoosts = Map.empty,
        domainIdBoosts = Map.empty[Int, Float],
        minShouldMatch = None,
        slop = None
      )
      val actual = JsonReader.fromString(request.toString)

      actual should be (expected)
      request.request.types should be (Array(esDocumentType))
    }

    "construct a multi match query with boosted fields" in {
      val query = j"""{
        "filtered": {
          "filter": ${defaultFilter},
          "query": ${boostedBoolQuery}
        }
      }"""

      val expected = j"""{
        "query": ${functionScoreQuery(query)}
      }"""

      val request = documentClient.buildBaseRequest(
        searchQuery = params.searchQuery,
        domains = Set.empty,
        searchContext = None,
        categories = None,
        tags = None,
        domainMetadata = None,
        only = None,
        fieldBoosts = params.fieldBoosts,
        datatypeBoosts = Map.empty,
        domainIdBoosts = Map.empty[Int, Float],
        minShouldMatch = None,
        slop = None
      )
      val actual = JsonReader.fromString(request.toString)

      actual should be (expected)
      request.request.types should be (Array(esDocumentType))
    }
  }

  ////////////////////
  // buildCountRequest
  // /////////////////

  // NOTE: ultimately, the functionScoreQuery does not belong in aggregations
  "buildCountRequest" should {
    "construct a filtered match query with nested aggregation for annotations" in {
      val query = j"""{
        "filtered": {
          "filter": ${complexFilter},
          "query": ${complexQuery}
        }
      }"""

      val expected = j"""{
        "size": 0,
        "query": ${query},
        "aggregations": {
            "annotations": {
                "aggregations": {
                    "names": {
                        "terms": {
                            "field": "animl_annotations.categories.name.raw",
                            "size": 0
                        }
                    }
                },
                "nested": {
                    "path": "animl_annotations.categories"
                }
            }
        },
        "query": ${functionScoreQuery(query)}
      }"""

      val request = documentClient.buildCountRequest(
        CategoriesFieldType,
        searchQuery = params.searchQuery,
        domains = Set.empty,
        searchContext = None,
        categories = params.categories,
        tags = params.tags,
        only = params.only
      )

      val actual = JsonReader.fromString(request.toString)

      actual should be (expected)
      request.request.searchType should be (COUNT)
    }
  }

  ////////////////////
  // buildFacetRequest
  ////////////////////

  // TODO: Make this a JSON comparison
  "buildFacetRequest" should {
    "build a faceted request" in {
      val domainId = 42
      val domain = Domain(
        isCustomerDomain = true,
        None,
        "",
        domainId,
        None,
        moderationEnabled = false,
        routingApprovalEnabled = false,
        lockedDown = false,
        apiLockedDown = false)

      val expectedAsString = s"""{
        |  "size" : 0,
        |  "aggregations" : {
        |    "domain_filter" : {
        |      "filter" : {
        |        "bool" : {
        |          "must" : [ {
        |            "terms" : {
        |              "socrata_id.domain_id" : [ $domainId ]
        |            }
        |          }, {
        |            "not" : {
        |              "filter" : {
        |                "term" : {
        |                  "is_public" : false
        |                }
        |              }
        |            }
        |          }, {
        |            "bool" : {
        |              "should" : [ {
        |                "term" : {
        |                  "is_default_view" : true
        |                }
        |              }, {
        |                "term" : {
        |                  "is_moderation_approved" : true
        |                }
        |              }, {
        |                "bool" : {
        |                  "must" : [ {
        |                    "terms" : {
        |                      "socrata_id.domain_id" : [ $domainId ]
        |                    }
        |                  }, {
        |                    "not" : {
        |                      "filter" : {
        |                        "terms" : {
        |                          "datatype" : [ "datalens", "datalens_chart", "datalens_map" ]
        |                        }
        |                      }
        |                    }
        |                  } ]
        |                }
        |              } ]
        |            }
        |          }, {
        |            "bool" : {
        |              "must" : {
        |                "bool" : {
        |                  "should" : [ {
        |                    "terms" : {
        |                      "socrata_id.domain_id" : [ $domainId ]
        |                    }
        |                  }, {
        |                    "term" : {
        |                      "is_approved_by_parent_domain" : true
        |                    }
        |                  } ]
        |                }
        |              }
        |            }
        |          } ]
        |        }
        |      },
        |      "aggregations" : {
        |        "datatypes" : {
        |          "terms" : {
        |            "field" : "datatype",
        |            "size" : 0
        |          }
        |        },
        |        "categories" : {
        |          "terms" : {
        |            "field" : "customer_category.raw",
        |            "size" : 0
        |          }
        |        },
        |        "tags" : {
        |          "terms" : {
        |            "field" : "customer_tags.raw",
        |            "size" : 0
        |          }
        |        },
        |        "metadata" : {
        |          "nested" : {
        |            "path" : "customer_metadata_flattened"
        |          },
        |          "aggregations" : {
        |            "keys" : {
        |              "terms" : {
        |                "field" : "customer_metadata_flattened.key.raw",
        |                "size" : 0
        |              },
        |              "aggregations" : {
        |                "values" : {
        |                  "terms" : {
        |                    "field" : "customer_metadata_flattened.value.raw",
        |                    "size" : 0
        |                  }
        |                }
        |              }
        |            }
        |          }
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin

      val actual = documentClient.buildFacetRequest(Some(domain))

      actual.toString should be(expectedAsString)
    }
  }

  /////////////////////
  // buildSearchRequest
  /////////////////////

  "buildSearchRequest" should {
    "add from, size, sort and only to a complex base request" in {
      val query = j"""{
        "filtered": {
          "filter": ${complexFilter},
          "query": ${complexQuery}
        }
      }"""

      val expected = j"""{
        "from": ${params.offset},
        "query": ${functionScoreQuery(query)},
        "size": ${params.limit},
        "sort": [ { "_score": {} }
        ]
      }"""

      val request = documentClient.buildSearchRequest(
        searchQuery = params.searchQuery,
        domains = Set.empty,
        searchContext = None,
        categories = params.categories,
        tags = params.tags,
        domainMetadata = None,
        only = params.only,
        fieldBoosts = Map.empty,
        datatypeBoosts = Map.empty,
        domainIdBoosts = Map.empty[Int, Float],
        minShouldMatch = None,
        slop = None,
        offset = params.offset,
        limit = params.limit,
        sortOrder = params.sortOrder
      )

      val actual = JsonReader.fromString(request.toString)

      actual should be (expected)
      request.toString.replaceAll("[\\s\\n]+", " ") should include(datatypeDatasetsFilter.toString())
    }

    "sort by average category scores given search context and categories" in {
      val query = j"""{
        "filtered": {
          "filter": ${complexFilter},
          "query": ${simpleQueryWithoutDomainFilter}
        }
      }"""

      val expected = j"""{
        "from": ${params.offset},
        "query": ${functionScoreQuery(query)},
        "size": ${params.limit},
        "sort": [
            {
                "animl_annotations.categories.score": {
                    "mode": "avg",
                    "nested_filter": {
                        "terms": {
                            "animl_annotations.categories.name.raw": [
                                "Social Services",
                                "Environment",
                                "Housing & Development"
                            ]
                        }
                    },
                    "order": "desc"
                }
            }
        ]
      }"""

      val request = documentClient.buildSearchRequest(
        searchQuery = NoQuery,
        domains = Set.empty,
        searchContext = None,
        categories = params.categories,
        tags = params.tags,
        domainMetadata = None,
        only = params.only,
        fieldBoosts = Map.empty,
        datatypeBoosts = Map.empty,
        domainIdBoosts = Map.empty[Int, Float],
        minShouldMatch = None,
        slop = None,
        offset = params.offset,
        limit = params.limit,
        sortOrder = params.sortOrder
      )

      val actual = JsonReader.fromString(request.toString)

      actual should be (expected)
      request.toString.replaceAll("[\\s\\n]+", " ") should include(datatypeDatasetsFilter.toString())
    }
  }

  ////////////////////////
  // generateAdvancedQuery
  ////////////////////////

  "generateAdvancedQuery" should {
    "produce an advanced query with field boosts applied" in {
      val expectedJson = j"""{
        "bool": {
          "should": [
            {
              "query_string": {
                "auto_generate_phrase_queries": true,
                "fields": [
                  "fts_analyzed",
                  "fts_raw",
                  "indexed_metadata.name^6.66",
                  "indexed_metadata.columns_description^1.11",
                  "indexed_metadata.columns_field_name^2.22",
                  "indexed_metadata.columns_name^3.33",
                  "datatype^4.44",
                  "indexed_metadata.description^5.55"
                ],
                "query": "any old query string"
              }
            },
            {
              "has_parent": {
                "parent_type": "domain",
                "query": {
                  "query_string": {
                    "auto_generate_phrase_queries": true,
                    "fields": [
                      "fts_analyzed",
                      "fts_raw",
                      "domain_cname",
                      "indexed_metadata.name^6.66",
                      "indexed_metadata.columns_description^1.11",
                      "indexed_metadata.columns_field_name^2.22",
                      "indexed_metadata.columns_name^3.33",
                      "datatype^4.44",
                      "indexed_metadata.description^5.55"
                    ],
                    "query": "any old query string"
                  }
                }
              }
            }
          ]
        }
      }"""

      val actual = documentClient.generateAdvancedQuery(
        "any old query string",
        Map(
          ColumnDescriptionFieldType -> 1.11f,
          ColumnFieldNameFieldType -> 2.22f,
          ColumnNameFieldType -> 3.33f,
          DatatypeFieldType -> 4.44f,
          DescriptionFieldType -> 5.55f,
          TitleFieldType -> 6.66f
        )
      )

      val actualJson = JsonReader.fromString(actual.toString)

      actualJson should be (expectedJson)
    }
  }

  //////////////////////
  // generateSimpleQuery
  //////////////////////

  "generateSimpleQuery" should {
    "produce anything but a simple query" in {
      val expectedJson = j"""{
        "bool" :
          {
            "must" :
              {
                "multi_match" :
                  {
                    "query" : "query string OR (query AND string)",
                    "fields" : [ "fts_analyzed", "fts_raw" ],
                    "type" : "cross_fields",
                    "minimum_should_match" : "20%"
                  }
              },
            "should" :
              [
                {
                  "multi_match" :
                    {
                      "query" : "query string OR (query AND string)",
                      "fields" :
                        [
                          "fts_analyzed",
                          "fts_raw",
                          "indexed_metadata.description^7.77",
                          "indexed_metadata.name^8.88"
                        ],
                      "type" : "phrase",
                      "slop" : 12
                    }
                },
                {
                  "term" : { "datatype" : { "value" : "datalens", "boost" : 9.99 } }
                },
                {
                  "term" : { "datatype" : { "value" : "datalens_map", "boost" : 10.1 } }
                }
              ]
          }
      }"""

      val actual = documentClient.generateSimpleQuery(
        "query string OR (query AND string)",
        Map(DescriptionFieldType -> 7.77f, TitleFieldType -> 8.88f), // test field boosts
        Map(TypeDatalenses -> 9.99f, TypeDatalensMaps -> 10.10f), // test type boosts
        Some("20%"), // minShouldMatch is a String because it can be a percentage
        Some(12) // slop is max num of intervening unmatched positions permitted
      )

      val actualJson = JsonReader.fromString(actual.toString)

      actualJson should be (expectedJson)
    }
  }

  ///////////////////////
  // chooseMinShouldMatch
  ///////////////////////

  "chooseMinShouldMatch" should {
    val msm = Some("2<-25% 9<-3") // I can be an involved string
    val sc = Domain(false, None, "example.com", 1, Some("Example! (dotcom)"), false, false, false, false)

    "choose minShouldMatch if present" in {
      documentClient.chooseMinShouldMatch(msm, None) should be (msm)
      documentClient.chooseMinShouldMatch(msm, Some(sc)) should be (msm)
    }

    // Use case here is increasing search precision on customer domains
    "choose default MSM value if none is passed in but search context is present" in {
      documentClient.chooseMinShouldMatch(None, Some(sc)) should be (defaultMinShouldMatch)
    }

    "choose nothing if no MSM value is passed in and no search context is present" in {
      documentClient.chooseMinShouldMatch(None, None) should be (None)
    }
  }
}
