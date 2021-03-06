package com.socrata.cetera.search

import com.rojoma.json.v3.interpolation._
import com.rojoma.json.v3.io.JsonReader
import org.elasticsearch.index.query.QueryBuilders
import org.scalatest.{ShouldMatchers, WordSpec}

import com.socrata.cetera.types.{Datatype, TypeDatalenses, TypeDatasets, TypeFilters}

class BoostsSpec extends WordSpec with ShouldMatchers {
  "applyDatatypeBoosts" should {
    "add datatype boosts to the query" in {
      val query = QueryBuilders.boolQuery()
      val datatypeBoosts = Map[Datatype, Float](
        TypeDatasets -> 1.23f,
        TypeDatalenses -> 2.34f,
        TypeFilters -> 0.98f
      )
      Boosts.applyDatatypeBoosts(query, datatypeBoosts)

      val expectedJson = j"""{
        "bool" : {
          "should" : [
            { "term" : { "datatype" : { "value" : "dataset", "boost" : 1.23 } } },
            { "term" : { "datatype" : { "value" : "datalens", "boost" : 2.34 } } },
            { "term" : { "datatype" : { "value" : "filter", "boost" : 0.98 } } }
          ]
        }
      }"""

      val actualJson = JsonReader.fromString(query.toString)
      actualJson should be (expectedJson)
    }

    "return empty bool if called with empty map" in {
      val query = QueryBuilders.boolQuery()
      val datatypeBoosts = Map.empty[Datatype, Float]
      Boosts.applyDatatypeBoosts(query, datatypeBoosts)

      val expectedJson = j"""{"bool" : { } }"""

      val actualJson = JsonReader.fromString(query.toString)
      actualJson should be(expectedJson)
    }
  }

  "boostDomains" should {
    "add domain cname boosts to the query" in {
      val query = QueryBuilders.functionScoreQuery(QueryBuilders.matchAllQuery)
      val domainBoosts = Map[Int, Float](
        0 -> 1.23f,
        7 -> 4.56f
      )

      Boosts.applyDomainBoosts(query, domainBoosts)

      val expectedJson = j"""{
        "function_score": {
          "functions": [
            {
              "filter": { "term": { "socrata_id.domain_id": 0 } },
              "weight": 1.23
            },
            {
              "filter": { "term": { "socrata_id.domain_id": 7 } },
              "weight": 4.56
            }
          ],
          "query": {
            "match_all": {}
          }
        }
      }"""

      val actualJson = JsonReader.fromString(query.toString)

      actualJson should be (expectedJson)
    }

    "do nothing to the query if given no domain boosts" in {
      val query = QueryBuilders.functionScoreQuery(QueryBuilders.matchAllQuery)
      val domainBoosts = Map.empty[Int, Float]

      val beforeJson = JsonReader.fromString(query.toString)
      Boosts.applyDomainBoosts(query, domainBoosts)
      val afterJson = JsonReader.fromString(query.toString)

      afterJson should be(beforeJson)
    }
  }
}
