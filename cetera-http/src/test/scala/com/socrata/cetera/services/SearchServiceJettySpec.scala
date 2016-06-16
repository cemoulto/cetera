package com.socrata.cetera.services

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.Collections
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.collection.JavaConverters._

import com.socrata.http.server.HttpRequest
import com.socrata.http.server.HttpRequest.AugmentedHttpServletRequest
import org.elasticsearch.action.search.SearchRequestBuilder
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}
import org.springframework.mock.web.{DelegatingServletInputStream, DelegatingServletOutputStream}

import com.socrata.cetera._
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.search.{BaseDocumentClient, BaseDomainClient}
import com.socrata.cetera.types.NoQuery

class SearchServiceJettySpec extends FunSuiteLike with Matchers with MockFactory with BeforeAndAfterAll {
  val testSuiteName = getClass.getSimpleName.toLowerCase
  val client = new TestESClient(testSuiteName)
  val mockDomainClient = mock[BaseDomainClient]
  val mockDocumentClient = mock[BaseDocumentClient]
  val balboaClient = new BalboaClient("/tmp/metrics")
  val service = new SearchService(mockDocumentClient, mockDomainClient, balboaClient)

  override protected def afterAll(): Unit = {
    client.close()
  }

  test("pass on any set-cookie headers in response") {
    val expectedSetCookie = Seq("life=42", "universe=42", "everything=42")

    mockDomainClient.expects('findRelevantDomains)(None, None, Some("c=cookie"), Some("1"))
      .returns((None, Set.empty, 123L, expectedSetCookie))

    mockDocumentClient.expects('buildSearchRequest)(NoQuery, Set(), None, None, None, None, None, None, None, None, None,
      Map(), Map(), Map(), None, None, 0, 100, None)
      .returns(new SearchRequestBuilder(client.client))

    val servReq = mock[HttpServletRequest]
    servReq.expects('getMethod)().anyNumberOfTimes.returns("GET")
    servReq.expects('getRequestURI)().anyNumberOfTimes.returns("/test")
    servReq.expects('getHeaderNames)().anyNumberOfTimes.returns(Collections.emptyEnumeration[String]())
    servReq.expects('getInputStream)().anyNumberOfTimes.returns(
      new DelegatingServletInputStream(new ByteArrayInputStream("".getBytes)))
    servReq.expects('getHeader)(HeaderCookieKey).returns("c=cookie")
    servReq.expects('getHeader)(HeaderXSocrataHostKey).anyNumberOfTimes.returns(null)
    servReq.expects('getHeader)(HeaderXSocrataRequestIdKey).anyNumberOfTimes.returns("1")
    servReq.expects('getQueryString)().anyNumberOfTimes.returns("")
    servReq.expects('getRemoteHost)().anyNumberOfTimes.returns("remotehost")

    val augReq = new AugmentedHttpServletRequest(servReq)
    val httpReq = mock[HttpRequest]
    httpReq.expects('servletRequest)().anyNumberOfTimes.returning(augReq)

    val outStream = new ByteArrayOutputStream()
    val response = mock[HttpServletResponse]
    val status: Integer = 200
    response.expects('setStatus)(status)
    response.expects('setHeader)("Access-Control-Allow-Origin", "*")
    response.expects('setContentType)("application/json; charset=UTF-8")
    expectedSetCookie.foreach { s => response.expects('addHeader)("Set-Cookie", s)}
    response.expects('getHeaderNames)().anyNumberOfTimes.returns(Seq("Set-Cookie").asJava)
    response.expects('getHeaders)("Set-Cookie").anyNumberOfTimes.returns(expectedSetCookie.asJava)
    response.expects('getOutputStream)().returns(new DelegatingServletOutputStream(outStream))

    service.search(httpReq)(response)
  }
}
