package com.socrata.cetera

import java.util.concurrent.Executors

import com.rojoma.simplearm.v2.Resource
import com.socrata.http.client.HttpClientHttpClient

import com.socrata.cetera.auth.CoreClient

class TestCoreClient(httpClient: TestHttpClient, port: Int) extends
  CoreClient(httpClient.client, "localhost", port, 2000, None)

class TestHttpClient {
  implicit val shutdownTimeout = Resource.executorShutdownNoTimeout
  val executor = Executors.newCachedThreadPool()
  val client = new HttpClientHttpClient(executor, HttpClientHttpClient.defaultOptions)

  def close(): Unit = {
    client.close()
    executor.shutdown()
  }
}
