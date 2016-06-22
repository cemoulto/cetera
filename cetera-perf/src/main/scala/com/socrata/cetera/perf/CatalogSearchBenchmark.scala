package com.socrata.cetera.perf

import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.JavaConverters._
import scala.util.Random

import com.rojoma.simplearm.v2.Resource
import com.socrata.http.client.HttpClientHttpClient
import com.typesafe.config.ConfigFactory
import org.elasticsearch.search.aggregations.AggregationBuilders.terms
import org.elasticsearch.search.aggregations.bucket.terms.Terms
import org.openjdk.jmh.annotations._

import com.socrata.cetera.auth.CoreClient
import com.socrata.cetera.config.CeteraConfig
import com.socrata.cetera.handlers.util._
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.search.{DocumentClient, DomainClient}
import com.socrata.cetera.services.{DomainCountService, SearchService}

// scalastyle:off magic.number
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 8)
@Measurement(iterations = 4)
@Threads(1)
@Fork(value = 1)
class CatalogSearchBenchmark {
  val config = new CeteraConfig(ConfigFactory.load())
  val client = new PerfESClient
  val balboaClient = new BalboaClient(config.balboa.dataDirectory)
  implicit val shutdownTimeout = Resource.executorShutdownNoTimeout
  val executor = Executors.newCachedThreadPool()
  val httpClient = new HttpClientHttpClient(executor, HttpClientHttpClient.defaultOptions)
  val coreClient = new CoreClient(httpClient, config.core.host, config.core.port,
    config.core.connectionTimeoutMs, config.core.appToken)
  val domainClient = new DomainClient(client, coreClient, config.elasticSearch.indexAliasName)
  val documentClient = new DocumentClient(
    client,
    domainClient,
    config.elasticSearch.indexAliasName,
    config.elasticSearch.titleBoost,
    config.elasticSearch.minShouldMatch,
    Set.empty // TODO: enable expression script lang to benchmark including function score scripts
  )
  val domainCountService = new DomainCountService(domainClient)
  val searchService = new SearchService(documentClient, domainClient, balboaClient)

  var queryParameters = Seq.empty[MultiQueryParams]
  var domainCnames = Seq.empty[String]

  @Setup(Level.Trial)
  def setupIndex(): Unit = {
    client.bootstrapData(10)

    val res = client.client.prepareSearch(config.elasticSearch.indexAliasName)
      .addAggregation(terms("domains").field("domain_cname.raw"))
      .execute.actionGet
    domainCnames = res.getAggregations.get[Terms]("domains").getBuckets.asScala.map(b => b.getKey)
    println(s"found domain cnames: $domainCnames")
  }

  @TearDown(Level.Trial)
  def teardownIndex(): Unit = {
    client.removeBootstrapData()
  }

  @Setup(Level.Iteration)
  def setupIteration(): Unit = {
    queryParameters = Range(0, 1000).map(i => fabricateQuery)
  }

  private def fabricateQuery: MultiQueryParams = {
    Map("q" -> Seq(Random.alphanumeric.take(Random.nextInt(64)).force.mkString))
  }

  @Benchmark
  def searchDomain(): Unit = {
    domainCnames.foreach(c => domainClient.findRelevantDomains(Some(c), None, None, None))
  }

  @Benchmark
  def query(): Unit = {
    queryParameters.foreach(q => searchService.doSearch(q, None, None, None))
  }
}
