package com.socrata.cetera.services

import com.rojoma.json.v3.ast.JString
import com.rojoma.json.v3.interpolation._
import com.rojoma.json.v3.io.CompactJsonWriter
import com.socrata.http.server.responses._
import org.mockserver.integration.ClientAndServer._
import org.mockserver.model.HttpRequest._
import org.mockserver.model.HttpResponse._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, Matchers}

import com.socrata.cetera._
import com.socrata.cetera.{TestCoreClient, TestESClient, TestESData, TestHttpClient}
import com.socrata.cetera.search.{DocumentClient, DomainClient}
import com.socrata.cetera.types._

class ProfileServiceSpec extends FunSuiteLike with Matchers with TestESData
  with BeforeAndAfterAll with BeforeAndAfterEach {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    bootstrapData()
  }

  override def beforeEach(): Unit = {
    mockCoreServer.reset()
  }

  override protected def afterAll(): Unit = {
    removeBootstrapData()
    client.close()
    mockCoreServer.stop(true)
    httpClient.close()
    super.afterAll()
  }

  val httpClient = new TestHttpClient()
  val coreTestPort = 8030
  val mockCoreServer = startClientAndServer(coreTestPort)
  val coreClient = new TestCoreClient(httpClient, coreTestPort)

  val client = new TestESClient(testSuiteName)
  val domainClient = new DomainClient(client, coreClient, testSuiteName)
  val documentClient = new DocumentClient(client, domainClient, testSuiteName, None, None, Set.empty)

  val service = new ProfileService(documentClient, domainClient, coreClient)

  test("authenticate rejects anonymous requests") {
    val (auth, _) = service.verifyUserAuthentication(None, "", None)
    auth should be(false)
  }

  test("search without authentication is rejected") {
    val (status, results, _, _) = service.doSearch(Map.empty, None, None, None)
    status should be(Unauthorized)
    results.results.headOption should be('empty)
  }

  test("search without domain is rejected") {
    val cookie = "Traditional = WASD"
    val (status, results, _, _) = service.doSearch(Map.empty, Some(cookie), None, None)
    status should be(Unauthorized)
    results.results.headOption should be('empty)
  }

  test("search with authentication returns datasets the user owns") {
    val cookie = "Traditional = WASD"
    val host = "petercetera.net"

    val authedUserBody =
      j"""{
        "id" : "robin-hood"
        }"""
    val expectedRequest = request()
      .withMethod("GET")
      .withPath("/users.json")
      .withHeader(HeaderXSocrataHostKey, host)
    mockCoreServer.when(
      expectedRequest
    ).respond(
      response()
        .withStatusCode(200)
        .withHeader("Content-Type", "application/json; charset=utf-8")
        .withBody(CompactJsonWriter.toString(authedUserBody))
    )

    val authedUserId = authedUserBody.dyn.id.!.asInstanceOf[JString].string
    val params = Map("for_user" -> authedUserId).mapValues(Seq(_))

    val (status, results, _, _) = service.doSearch(params, Some(cookie), Some(host), None)

    mockCoreServer.verify(expectedRequest)
    status should be(OK)
    results.results.headOption should be('defined)

    val actualFxfs = results.results.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    val expectedFxfs = Seq("fxf-0", "fxf-4", "fxf-8", "zeta-0001", "zeta-0003", "zeta-0006")
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

  test("search with authentication returns datasets the user is shared on") {
    val cookie = "Traditional = WASD"
    val host = "petercetera.net"

    val authedUserBody =
      j"""{
        "id" : "robin-hood"
        }"""
    val expectedRequest = request()
      .withMethod("GET")
      .withPath("/users.json")
      .withHeader(HeaderXSocrataHostKey, host)
    mockCoreServer.when(
      expectedRequest
    ).respond(
      response()
        .withStatusCode(200)
        .withHeader("Content-Type", "application/json; charset=utf-8")
        .withBody(CompactJsonWriter.toString(authedUserBody))
    )

    val authedUserId = authedUserBody.dyn.id.!.asInstanceOf[JString].string
    val params = Map("recipient" -> authedUserId).mapValues(Seq(_))

    val (status, results, _, _) = service.doSearch(params, Some(cookie), Some(host), None)

    mockCoreServer.verify(expectedRequest)
    status should be(OK)
    results.results.headOption should be('defined)

    val actualFxfs = results.results.map(_.resource.dyn.id.!.asInstanceOf[JString].string)
    val expectedFxfs = Seq("zeta-0004")
    actualFxfs should contain theSameElementsAs expectedFxfs
  }

}
