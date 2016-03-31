package com.socrata.cetera.services

import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import com.socrata.cetera.search._
import com.socrata.cetera.types.Count
import com.socrata.cetera.util.Params
import com.socrata.cetera.{TestESClient, TestESData}

class DomainCountServiceSpec extends FunSuiteLike with Matchers with BeforeAndAfterAll with TestESData {
  val client: ElasticSearchClient = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val service: DomainCountService = new DomainCountService(domainClient)

  override protected def beforeAll(): Unit = {
    bootstrapData()
  }

  override protected def afterAll(): Unit = {
    client.close()
  }

  test("count domains from unmoderated search context") {
    val expectedResults = List(
      Count("annabelle.island.net", 1),
      Count("blue.org", 1),
      Count("opendata-demo.socrata.com", 1),
      Count("petercetera.net", 4))
    val (res, _) = service.doAggregate(Map(
      Params.context -> "petercetera.net",
      Params.filterDomains -> "petercetera.net,opendata-demo.socrata.com,blue.org,annabelle.island.net")
      .mapValues(Seq(_)), None)
    res.results should contain theSameElementsAs expectedResults
  }

  test("count domains from moderated search context") {
    val expectedResults = List(
      Count("annabelle.island.net", 1),
      Count("blue.org", 0),
      Count("opendata-demo.socrata.com", 1),
      Count("petercetera.net", 1))
    val (res, _) = service.doAggregate(Map(
      Params.context -> "annabelle.island.net",
      Params.filterDomains -> "petercetera.net,opendata-demo.socrata.com,blue.org,annabelle.island.net")
      .mapValues(Seq(_)), None)
    res.results should contain theSameElementsAs expectedResults
  }

  test("count domains default to include only customer domains") {
    val expectedResults = List(
      Count("annabelle.island.net", 1),
      Count("blue.org", 1),
      Count("dylan.demo.socrata.com", 0),
      // opendata-demo.socrata.com is not a customer domain, so the domain and all docs should be hidden
      // Count("opendata-demo.socrata.com", 0),
      Count("petercetera.net", 4))
    val (res, _) = service.doAggregate(Map.empty, None)
    res.results should contain theSameElementsAs expectedResults
  }
}
