package com.socrata.cetera.services

import com.rojoma.json.v3.ast.JString
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import com.socrata.cetera._
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.search._
import com.socrata.cetera.util.Params

class DomainBoostSpec extends FunSuiteLike with Matchers with TestESData with BeforeAndAfterAll {
  val client: ElasticSearchClient = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)
  val documentClient: DocumentClient = new DocumentClient(client, domainClient, testSuiteName, None, None, Set.empty)
  val balboaClient: BalboaClient = new BalboaClient("/tmp/metrics")
  val service: SearchService = new SearchService(documentClient, domainClient, balboaClient)

  override protected def beforeAll(): Unit = {
    bootstrapData()
  }

  override protected def afterAll(): Unit = {
    removeBootstrapData()
    client.close()
  }

  val activeDomainCnames = Seq("petercetera.net", "blue.org")

  test("domain boost of 2 should produce top result") {
    activeDomainCnames.foreach { domain =>
      val (results, _) = service.doSearch(Map(
        Params.showScore -> "true",
        Params.context -> domain,
        Params.filterDomains -> activeDomainCnames.mkString(","),
        s"""boostDomains[${domain}]""" -> "2").mapValues(Seq(_)), None)
      results.results.head.metadata(esDomainType) should be(JString(domain))
    }
  }

  test("domain boost of 0.5 should not produce top result") {
    activeDomainCnames.foreach { domain =>
      val (results, _) = service.doSearch(Map(
        Params.showScore -> "true",
        Params.context -> domain,
        Params.filterDomains -> activeDomainCnames.mkString(","),
        s"""boostDomains[${domain}]""" -> "0.5").mapValues(Seq(_)), None)
      results.results.head.metadata(esDomainType) shouldNot be(JString(domain))
    }
  }

  test("domain boosts are not mistaken for custom metadata") {
    activeDomainCnames.foreach { domain =>
      val (results, _) = service.doSearch(
        Map(s"""boostDomains[${domain}]""" -> "2.34",
            "boostDomains[]" -> "3.45", // if I were custom metadata, I would not match any documents
            Params.context -> domain).mapValues(Seq(_)),
        None
      )
      results.results.size should be > 0
    }
  }

  // just documenting behavior; this may not be ideal behavior
  test("boostDomains with no [] is treated as custom metadata") {
    activeDomainCnames.foreach { domain =>
      val (results, _) = service.doSearch(
        Map(s"""boostDomains[${domain}]""" -> "2.34",
            "boostDomains" -> "3.45", // interpreted as custom metadata field which doesn't match any documents
            Params.context -> domain).mapValues(Seq(_)),
        None
      )
      results.results should contain theSameElementsAs List.empty
    }
  }
}
