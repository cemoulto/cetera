package com.socrata.cetera.perf

import java.io.File
import java.nio.file.Files
import scala.util.Random

import com.rojoma.json.v3.util.{JsonKey, JsonUtil}
import org.elasticsearch.client.{Client, Requests}
import org.elasticsearch.common.settings.ImmutableSettings
import org.elasticsearch.node.NodeBuilder._
import org.joda.time.DateTime

import com.socrata.cetera.esDomainType
import com.socrata.cetera.esDocumentType
import com.socrata.cetera.search._
import com.socrata.cetera.types._
import com.socrata.cetera.util.ElasticsearchBootstrap

// scalastyle:off magic.number
class PerfESClient(testSuiteName: String = "catalog")
  extends ElasticSearchClient("", 0, "", testSuiteName) { // host:port & cluster name are immaterial
  val tempDataDir = Files.createTempDirectory("elasticsearch_data_").toFile
  val local: Boolean = true
  val testSettings = ImmutableSettings.settingsBuilder()
    .put(settings)
    .put("path.data", tempDataDir.toString)
    .put("script.expressions.sandbox.enabled", true)
    .put("script.groovy.sandbox.enabled", true)
    .put("script.inline", "on")
    .put("script.indexed", "on")
    .put("script.file", "on")
    .put("script.search", "on")
    .build
  val node = nodeBuilder().settings(testSettings).local(local).node()
  override val client: Client = node.client()

  private def fabricateDocument(domainId: Int, docId: Int): Document = {
    val fxf = s"$domainId-$docId"
    val name = Random.alphanumeric.take(8).force.mkString
    val description = Random.alphanumeric.take(64).force.mkString
    val datatype = Random.shuffle(Datatypes.materialized).head.singular
    val viewtype = if(Random.nextBoolean()) "datalens" else ""
    val approved = Random.nextBoolean()

    Document(
      SocrataId(fxf, None, domainId),
      Resource(
        description,
        fxf,
        None, // parent ID
        new DateTime(Random.nextLong()).toString,
        new DateTime(Random.nextLong()).toString,
        datatype,
        fxf,
        Seq.empty, // columns
        name),
      AnimlAnnotations(Seq.empty, Seq.empty),
      datatype,
      viewtype,
      Some(Random.nextFloat()),
      IndexedMetadata(name, description, Seq.empty, Seq.empty, Seq.empty),
      Map.empty,
      Random.nextBoolean(),
      if (Random.nextBoolean()) Some(Random.nextBoolean()) else None,
      if (approved) Seq(domainId) else Seq.empty,
      approved,
      PageViews(Random.nextLong(), Random.nextLong(), Random.nextLong()),
      Random.alphanumeric.take(12).force.mkString,
      Seq.empty,
      None
    )
  }

  private val subdomains = Seq("data", "opendata")
  private val tlds = Seq("com", "gov", "org")
  private def fabricateDomain(domainId: Int): Domain = {
    val domainCname = Seq(
      Random.shuffle(subdomains).head,
      Random.alphanumeric.take(Random.nextInt(39) + 3).force.mkString,
      Random.shuffle(tlds).head
    ).mkString(".")
    Domain(
      domainId,
      domainCname,
      Some(Random.alphanumeric.take(16).force.mkString),
      Some(Random.alphanumeric.take(8).force.mkString),
      Random.nextBoolean(),
      Random.nextBoolean(),
      Random.nextBoolean(),
      Random.nextBoolean(),
      Random.nextBoolean()
    )
  }

  private def indexDomain(domain: Domain): Unit = {
    client.prepareIndex(testSuiteName, esDomainType)
      .setSource(JsonUtil.renderJson(domain))
      .execute.actionGet
  }

  private def indexDocument(document: Document): Unit = {
    client.prepareIndex(testSuiteName, esDocumentType)
      .setSource(JsonUtil.renderJson(document))
      .setParent(document.socrataId.domainId.toString)
      .execute.actionGet
  }

  def bootstrapData(domainCount: Int): Unit = {
    println("bootstrap creating index settings and mappings.")
    ElasticsearchBootstrap.ensureIndex(this, "yyyyMMddHHmm", testSuiteName)

    Range(0, domainCount).foreach { i =>
      val docCount = 950 + Random.nextInt(100)
      println(s"bootstrap indexing domain $i with $docCount documents.")
      indexDomain(fabricateDomain(i))
      Range(0, docCount).foreach { j =>
        indexDocument(fabricateDocument(i, j))
      }
    }
  }

  def removeBootstrapData(): Unit = {
    deleteIndex()
  }

  override def close(): Unit = {
    node.close()

    try { // don't care if cleanup succeeded or failed
      def deleteR(file: File): Unit = {
        if (file.isDirectory) Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(deleteR)
        file.delete()
      }
      deleteR(tempDataDir)
    } catch { case e: Exception => () }

    super.close()
  }

  def deleteIndex(): Unit =
    client.admin.indices.delete(Requests.deleteIndexRequest(testSuiteName))
      .actionGet
}
