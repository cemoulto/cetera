package com.socrata.cetera.search

import org.scalatest.{BeforeAndAfterAll, ShouldMatchers, WordSpec}

import com.socrata.cetera.types.Domain
import com.socrata.cetera.{TestESClient, TestESData}

class DomainClientSpec extends WordSpec with ShouldMatchers with TestESData with BeforeAndAfterAll {
  val client = new TestESClient(testSuiteName)
  val domainClient: DomainClient = new DomainClient(client, null, testSuiteName)

  override protected def beforeAll(): Unit = {
    bootstrapData()
  }

  override protected def afterAll(): Unit = {
    removeBootstrapData()
    client.close()
  }

  "findRelevantDomains" should {
    "return the domain if it exists : petercetera.net" in {
      val expectedDomain = Domain(
        isCustomerDomain = true,
        organization = Some(""),
        domainCname = "petercetera.net",
        domainId = 0,
        siteTitle = Some("Temporary URI"),
        moderationEnabled = false,
        routingApprovalEnabled = false,
        lockedDown = false,
        apiLockedDown = false
      )
      val (actualDomains, _) = domainClient.findRelevantDomains(Some("petercetera.net"), Set("petercetera.net"), None)
      actualDomains.headOption.get should be(expectedDomain)
    }

    "return the domain if it exists : opendata-demo.socrata.com" in {
      val expectedDomain = Domain(
        isCustomerDomain = false,
        organization = Some(""),
        domainCname = "opendata-demo.socrata.com",
        domainId = 1,
        siteTitle = Some("And other things"),
        moderationEnabled = true,
        routingApprovalEnabled = false,
        lockedDown = false,
        apiLockedDown = false
      )
      val (actualDomains, _) = domainClient.findRelevantDomains(Some("opendata-demo.socrata.com"),
        Set("opendata-demo.socrata.com"), None)
      actualDomains.headOption.get should be(expectedDomain)
    }

    "return None if the domain does not exist" in {
      val expectedDomain = None
      val (actualDomains, _) = domainClient.findRelevantDomains(Some("hellcat.com"), Set("hellcat.socrata.com"), None)
      actualDomains.headOption should be(expectedDomain)
    }

    "return None if searching for blank string" in {
      val expectedDomain = None
      val (actualDomains, _) = domainClient.findRelevantDomains(Some(""), Set(""), None)
      actualDomains.headOption should be(expectedDomain)
    }

    "return only domains with an exact match" in {
      val expectedDomain = Domain(
        isCustomerDomain = true,
        organization = Some(""),
        domainCname = "dylan.demo.socrata.com",
        domainId = 4,
        siteTitle = Some("Temporary URI"),
        moderationEnabled = false,
        routingApprovalEnabled = false,
        lockedDown = false,
        apiLockedDown = false)

      val (actualDomains, _) = domainClient.findRelevantDomains(Some("dylan.demo.socrata.com"),
        Set("dylan.demo.socrata.com"), None)
      actualDomains.headOption shouldBe Some(expectedDomain)
    }
  }
}
