package com.socrata.cetera.search

import org.scalatest.{BeforeAndAfterAll, ShouldMatchers, WordSpec}

import com.socrata.cetera.types.{AdvancedQuery, Domain, NoQuery, SimpleQuery}

// NOTE: The toString method of the SortBuilders does not produce
// JSON-parseable output. So, we test the output of the toString method as a
// proxy for equality.

class SortsSpec extends WordSpec with ShouldMatchers with BeforeAndAfterAll {
  "sortScoreDesc" should {
    "sort by score descending" in {
      val expectedAsString = s"""
        |"_score"{ }""".stripMargin

      val actual = Sorts.sortScoreDesc

      actual.toString should be (expectedAsString)
    }
  }

  "sortFieldAsc" should {
    "sort by a given field ascending" in {
      val field = "field.in.our.documents"
      val expectedAsString = s"""
        |"${field}"{
        |  "order" : "asc",
        |  "missing" : "_last"
        |}""".stripMargin

      val actual = Sorts.sortFieldAsc(field)

      actual.toString should be(expectedAsString)
    }
  }

  "sortFieldDesc" should {
    "sort by a given field descending" in {
      val field = "another_field.another_field_total"
      val expectedAsString = s"""
        |"${field}"{
        |  "order" : "desc",
        |  "missing" : "_last"
        |}""".stripMargin

      val actual = Sorts.sortFieldDesc(field)

      actual.toString should be(expectedAsString)
    }
  }

  "buildAverageScoreSort" should {
    "build a sort by average field score descending" in {
      val fieldName = "this_looks_like.things"
      val rawFieldName = "this_looks_like.things.raw"
      val classifications = Set("one kind of thing", "another kind of thing")

      // Using toString as proxy since toString does not give JSON parsable string
      val expectedAsString = s"""
         |"${fieldName}"{
         |  "order" : "desc",
         |  "missing" : "_last",
         |  "mode" : "avg",
         |  "nested_filter" : {
         |    "terms" : {
         |      "${rawFieldName}" : [ "${classifications.head}", "${classifications.last}" ]
         |    }
         |  }
         |}""".stripMargin

      val actual = Sorts.buildAverageScoreSort(fieldName, rawFieldName, classifications)

      actual.toString should be (expectedAsString)
    }
  }

  "chooseSort" should {
    val cats = Set[String]("comestibles", "potables")
    val tags = Set[String]("tasty", "sweet", "taters", "precious")

    "order by query score descending when given an advanced query" in {
      val expected = Sorts.sortScoreDesc

      val actual = Sorts.chooseSort(
        searchQuery = AdvancedQuery("sandwich AND (soup OR salad)"),
        searchContext = None,
        categories = None,
        tags = Some(tags)
      )

      // sortScores is a val so it's the same object
      actual should be(expected)
    }

    "order by score desc when given a simple query" in {
      val expected = Sorts.sortScoreDesc

      val searchContext = Domain(
        domainId = 1,
        domainCname = "peterschneider.net",
        siteTitle = Some("Temporary URI"),
        organization = Some("SDP"),
        isCustomerDomain = false,
        moderationEnabled = false,
        routingApprovalEnabled = true,
        lockedDown = false,
        apiLockedDown = false
      )

      val actual = Sorts.chooseSort(
        searchQuery = SimpleQuery("soup salad sandwich"),
        searchContext = Some(searchContext),
        categories = Some(cats),
        tags = None
      )

      // sortScores is a val so it's the same object
      actual should be(expected)
    }

    "order by average category score descending when no query but ODN categories present" in {
      val expectedAsString = s"""
        |"animl_annotations.categories.score"{
        |  "order" : "desc",
        |  "missing" : "_last",
        |  "mode" : "avg",
        |  "nested_filter" : {
        |    "terms" : {
        |      "animl_annotations.categories.name.raw" : [ "${cats.head}", "${cats.last}" ]
        |    }
        |  }
        |}""".stripMargin

      val actual = Sorts.chooseSort(
        searchQuery = NoQuery,
        searchContext = None,
        categories = Some(cats),
        tags = Some(tags)
      )

      actual.toString should be(expectedAsString)
    }

    "order by average tag score desc when no query or categories but ODN tags present" in {
      val tagsJson = "[ \"" + tags.mkString("\", \"") + "\" ]"

      val expectedAsString = s"""
        |"animl_annotations.tags.score"{
        |  "order" : "desc",
        |  "missing" : "_last",
        |  "mode" : "avg",
        |  "nested_filter" : {
        |    "terms" : {
        |      "animl_annotations.tags.name.raw" : ${tagsJson}
        |    }
        |  }
        |}""".stripMargin

      val actual = Sorts.chooseSort(
        searchQuery = NoQuery,
        searchContext = None,
        categories = None,
        tags = Some(tags)
      )

      actual.toString should be(expectedAsString)
    }

    "order by score descending for default null query" in {
      val expected = Sorts.sortScoreDesc

      val actual = Sorts.chooseSort(
        searchQuery = NoQuery,
        searchContext = None,
        categories = None,
        tags = None
      )

      actual should be (expected)
    }
  }

  "Sorts.paramSortMap" should {
    val order_asc = "\"order\" : \"asc\""
    val order_desc = "\"order\" : \"desc\""

    def testSortOrder(keys: Iterable[String], order: String): Unit = {
      keys.foreach { key =>
        Sorts.paramSortMap.get(key) match {
          case Some(sort) => sort.toString should include (order)
          case None => fail(s"missing sort order key: ${key}")
        }
      }
    }

    "explicit ascending sorts are all ascending" in {
      val ascending_sort_keys = Sorts.paramSortMap.keys.filter { k => k.endsWith("ASC") }
      testSortOrder(ascending_sort_keys, order_asc)
    }

    "explicit descending sorts are all descending" in {
      val descending_sort_keys = Sorts.paramSortMap.keys.filter { k => k.endsWith("DESC") }
      testSortOrder(descending_sort_keys,  order_desc)
    }

    "default ascending sorts are all ascending" in {
      val default_ascending_sort_keys = Seq[String]("name")
      testSortOrder(default_ascending_sort_keys, order_asc)
    }

    "default descending sorts are all descending" in {
      val default_descending_sort_keys = Seq[String]("createdAt",
        "updatedAt",
        "page_views_last_week",
        "page_views_last_month",
        "page_views_total")

      testSortOrder(default_descending_sort_keys, order_desc)
    }
  }
}
