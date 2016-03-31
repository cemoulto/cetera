package com.socrata.cetera.services

import com.rojoma.json.v3.ast.{JNumber, JString}
import com.rojoma.json.v3.codec.DecodeError
import com.rojoma.json.v3.interpolation._
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import com.socrata.cetera.{TestESData, TestESClient}
import com.socrata.cetera.search._
import com.socrata.cetera.types._
import com.socrata.cetera.util.SearchResults

class CountServiceSpec extends FunSuiteLike with Matchers with BeforeAndAfterAll {
  val testSuiteName = getClass.getSimpleName.toLowerCase
  val client: ElasticSearchClient = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val documentClient: DocumentClient = new DocumentClient(client, domainClient, testSuiteName, None, None, Set.empty)
  val service: CountService = new CountService(documentClient, domainClient)

  override protected def afterAll(): Unit = {
    client.close() // Important!!
  }

  val esResponse = j"""{
    "took" : 1,
    "timed_out" : false,
    "_shards" : {
      "total" : 10,
      "successful" : 10,
      "failed" : 0
    },
    "hits" : {
      "total" : 1862,
      "max_score" : 0.0,
      "hits" : [ ]
    },
    "aggregations" : {
      "domains" : {
        "doc_count_error_upper_bound" : 0,
        "sum_other_doc_count" : 0,
        "buckets" : [ {
          "key" : "onethousand.example.com",
          "doc_count" : 1000
        }, {
          "key" : "two-thirty-four.example.com",
          "doc_count" : 234
        }, {
          "key" : "seven-ate-nine.com",
          "doc_count" : 78
        }, {
          "key" : "poor-bono.example.com",
          "doc_count" : 1
        } ]
      }
    }
  }"""

  test("extract") {
    val expected = Stream(
      j"""{ "key" : "onethousand.example.com", "doc_count" : 1000 }""",
      j"""{ "key" : "two-thirty-four.example.com", "doc_count" : 234 }""",
      j"""{ "key" : "seven-ate-nine.com", "doc_count" : 78 }""",
      j"""{ "key" : "poor-bono.example.com", "doc_count" : 1 }"""
    )

    service.extract(esResponse) match {
      case Right(actual) =>
        (actual, expected).zipped.foreach{ (a, e) => a should be(e) }

      case Left(e) =>
        fail(e.toString)
    }
  }

  test("format") {
    val expected = SearchResults[Count](
      List(
        Count(JString("onethousand.example.com"), JNumber(1000)),
        Count(JString("two-thirty-four.example.com"),  JNumber(234)),
        Count(JString("seven-ate-nine.com"),  JNumber(78)),
        Count(JString("poor-bono.example.com"),  JNumber(1))
      )
    )

    service.extract(esResponse) match {
      case Right(extracted) =>
        val formatted = service.format(extracted)
        val results = formatted.results
        results.zip(expected.results).foreach{ case (a, e) => a should be (e) }

      case Left(e) =>
        fail(e.toString)
    }
  }

  test("return an error when the expected path to resources does not exist") {
    val body = j"""{}"""
    service.extract(body) match {
      case Right(aggregations) => fail("We should have returned a decode error!")
      case Left(error) => error match {
        case _: DecodeError =>
        case _ => fail("Expected a DecodeError")
      }
    }
  }

  ignore("timings") {}
  ignore("query parameters parser - errors") {}

}


class CountServiceSpecWithTestESData extends FunSuiteLike with Matchers with BeforeAndAfterAll with TestESData {
  val client: ElasticSearchClient = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val documentClient: DocumentClient = new DocumentClient(client, domainClient, testSuiteName, None, None, Set.empty)
  val service: CountService = new CountService(documentClient, domainClient)

  override protected def beforeAll(): Unit = {
    bootstrapData()
  }

  override protected def afterAll(): Unit = {
    client.close()
  }

  test("categories count request") {
    val expectedResults = List(Count("Personal", 4))
    val (res, _) = service.doAggregate(CategoriesFieldType, Map.empty, None)
    res.results should contain theSameElementsAs expectedResults
  }

  test("tags count request") {
    val expectedResults = List(Count("Happy", 4), Count("Accident", 4))
    val (res, _) = service.doAggregate(TagsFieldType, Map.empty, None)
    res.results should contain theSameElementsAs expectedResults
  }

  test("domain categories count request") {
    val expectedResults = List(Count("Alpha", 3), Count("Gamma", 1), Count("Fun", 2))
    val (res, _) = service.doAggregate(DomainCategoryFieldType, Map.empty, None)
    res.results should contain theSameElementsAs expectedResults
  }

  test("domain tags count request") {
    val expectedResults = List(Count("1-one",3), Count("3-three",1))
    val (res, _) = service.doAggregate(DomainTagsFieldType, Map.empty, None)
    res.results should contain theSameElementsAs expectedResults
  }
}
