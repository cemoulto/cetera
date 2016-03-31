package com.socrata.cetera

import java.util.concurrent.{ExecutorService, Executors}

import com.rojoma.simplearm.v2._
import com.socrata.http.client.{HttpClientHttpClient, InetLivenessChecker}
import com.socrata.http.server.SocrataServerJetty
import com.socrata.thirdparty.typesafeconfig.Propertizer
import com.typesafe.config.ConfigFactory
import org.apache.log4j.PropertyConfigurator
import org.elasticsearch.client.transport.TransportClient
import org.slf4j.LoggerFactory

import com.socrata.cetera.authentication.CoreClient
import com.socrata.cetera.config.CeteraConfig
import com.socrata.cetera.handlers.Router
import com.socrata.cetera.metrics.BalboaClient
import com.socrata.cetera.search.{DocumentClient, DomainClient, ElasticSearchClient}
import com.socrata.cetera.services._
import com.socrata.cetera.types.ScriptScoreFunction

// $COVERAGE-OFF$ jetty wiring
object SearchServer extends App {
  val config = new CeteraConfig(ConfigFactory.load())
  PropertyConfigurator.configure(Propertizer("log4j", config.log4j))

  val logger = LoggerFactory.getLogger(getClass)

  logger.info("Starting Cetera server on port {}... ", config.server.port)
  logger.info("Configuration:\n" + config.debugString)

  implicit def executorResource[A <: ExecutorService]: Resource[A] = new Resource[A] {
    def close(a: A): Unit = {
      a.shutdown()
    }
  }

  trait Startable { def start(): Unit }
  def managedStartable[T <: Startable : Resource](resource: => T): Managed[T] = new Managed[T] {
    override def run[A](f: T => A): A =
      using(resource) { r =>
        r.start()
        f(r)
      }
  }

  for {
    executor <- managed(Executors.newCachedThreadPool())

    livenessChecker <- managedStartable(
      new InetLivenessChecker(config.http.liveness.interval,
                              config.http.liveness.range,
                              config.http.liveness.missable,
                              executor) with Startable)

    esClient <- managed(
      ElasticSearchClient(config.elasticSearch))

    httpClient <- managed(
      new HttpClientHttpClient(executor, HttpClientHttpClient.defaultOptions.withUserAgent("cetera")))

    coreClient = new CoreClient(httpClient, config.core.host, config.core.port, config.core.connectionTimeoutMs,
      config.core.appToken)
    } {

    val domainClient = new DomainClient(esClient, coreClient, config.elasticSearch.indexAliasName)
    val documentClient = new DocumentClient(
      esClient,
      domainClient,
      config.elasticSearch.indexAliasName,
      config.elasticSearch.titleBoost,
      config.elasticSearch.minShouldMatch,
      config.elasticSearch.functionScoreScripts.flatMap(fnName =>
      ScriptScoreFunction.getScriptFunction(fnName)).toSet
    )

    logger.info("ElasticSearchClient initialized on nodes " +
                  esClient.client.asInstanceOf[TransportClient].transportAddresses().toString)

    logger.info("Initializing BalboClient")
    val balboaClient = new BalboaClient(config.balboa.dataDirectory)

    logger.info("Initializing VersionService")
    val versionService = VersionService

    logger.info("Initializing SearchService with document, domain and balboa clients")
    val searchService = new SearchService(documentClient, domainClient, balboaClient)

    logger.info("Initializing FacetService with document client")
    val facetService = new FacetService(documentClient, domainClient)

    logger.info("Initializing DomainCountService with domain client")
    val domainCountService = new DomainCountService(domainClient)

    logger.info("Initializing CountService with document and domain clients")
    val countService = new CountService(documentClient, domainClient)

    logger.info("Initializing router with services")
    val router = new Router(
      versionService.Service,
      searchService.Service,
      facetService.Service,
      domainCountService.Service,
      countService.Service
    )

    logger.info("Initializing handler")
    val handler = router.route _

    logger.info("Initializing server")
    val server = new SocrataServerJetty(
      handler,
      SocrataServerJetty
        .defaultOptions
        .withPort(config.server.port)
        .withGracefulShutdownTimeout(config.server.gracefulShutdownTimeout)
    )

    logger.info("Running server!")
    server.run()
  }

  logger.info("All done!")
}
// $COVERAGE-ON$
