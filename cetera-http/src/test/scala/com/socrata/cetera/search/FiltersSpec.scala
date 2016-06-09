package com.socrata.cetera.search

import com.rojoma.json.v3.interpolation._
import com.rojoma.json.v3.io.JsonReader
import org.scalatest.{ShouldMatchers, WordSpec}

class FiltersSpec extends WordSpec with ShouldMatchers {

  "DocumentQueries: categoriesQuery" should {
    "return the expected query if no categories are given" in {
      val categoryQuery = DocumentQueries.categoriesQuery(None)
      categoryQuery should be(None)
    }

    "return the expected query if one category is given" in {
      val categoryQuery = DocumentQueries.categoriesQuery(Some(Set("Fun")))
      val expected =
        j"""{"nested": {"query": {"bool": {"should":
          {"match": { "animl_annotations.categories.name": { "query": "Fun", "type": "phrase" }}},
         "minimum_should_match": "1" }}, "path": "animl_annotations.categories"}
      }"""
      val actual = JsonReader.fromString(categoryQuery.get.toString)
      actual should be(expected)
    }

    "return the expected query if multiple categories are given" in {
      val categoryQuery = DocumentQueries.categoriesQuery(Some(Set("Fun", "Times")))
      val expected =
        j"""{"nested": {"query": {"bool": {"should": [
          {"match": { "animl_annotations.categories.name": { "query": "Fun", "type": "phrase" }}},
          {"match": { "animl_annotations.categories.name": { "query": "Times", "type": "phrase" }}}
         ], "minimum_should_match": "1" }}, "path": "animl_annotations.categories"}
      }"""
      val actual = JsonReader.fromString(categoryQuery.get.toString)
      actual should be(expected)
    }

    "return the expected query if one category with multiple terms is given" in {
      val categoryQuery = DocumentQueries.categoriesQuery(Some(Set("Unemployment Insurance")))
      val expected =
        j"""{"nested": {"query": {"bool": {"should":
          {"match": { "animl_annotations.categories.name": { "query": "Unemployment Insurance", "type": "phrase" }}},
         "minimum_should_match": "1" }}, "path": "animl_annotations.categories"}
      }"""
      val actual = JsonReader.fromString(categoryQuery.get.toString)
      actual should be(expected)
    }
  }

  "DocumentQueries: tagsQuery" should {
    "return the expected query if no tags are given" in {
      val tagQuery = DocumentQueries.tagsQuery(None)
      tagQuery should be(None)
    }

    "return the expected query if one tag is given" in {
      val tagQuery = DocumentQueries.tagsQuery(Some(Set("Fun")))
      val expected =
        j"""{"nested": {"query": {"bool": {"should":
          {"match": { "animl_annotations.tags.name": { "query": "Fun", "type": "phrase" }}},
         "minimum_should_match": "1" }}, "path": "animl_annotations.tags"}
      }"""
      val actual = JsonReader.fromString(tagQuery.get.toString)
      actual should be(expected)
    }

    "return the expected query if multiple tags are given" in {
      val tagQuery = DocumentQueries.tagsQuery(Some(Set("Fun", "Times")))
      val expected =
        j"""{"nested": {"query": {"bool": {"should": [
          {"match": { "animl_annotations.tags.name": { "query": "Fun", "type": "phrase" }}},
          {"match": { "animl_annotations.tags.name": { "query": "Times", "type": "phrase" }}}
         ], "minimum_should_match": "1" }}, "path": "animl_annotations.tags"}
      }"""
      val actual = JsonReader.fromString(tagQuery.get.toString)
      actual should be(expected)
    }
  }

  "DocumentQueries: domainCategoriesQuery" should {
    "return the expected query if no categories are given" in {
      val categoryQuery = DocumentQueries.domainCategoriesQuery(None)
      categoryQuery should be(None)
    }

    "return the expected query if one category is given" in {
      val categoryQuery = DocumentQueries.domainCategoriesQuery(Some(Set("Fun")))
      val expected =
        j"""{"bool": {"should":
          {"match": { "customer_category": { "query": "Fun", "type": "phrase" }}},
         "minimum_should_match": "1" }
      }"""
      val actual = JsonReader.fromString(categoryQuery.get.toString)
      actual should be(expected)
    }

    "return the expected query if multiple categories are given" in {
      val categoryQuery = DocumentQueries.domainCategoriesQuery(Some(Set("Fun", "Times")))
      val expected =
        j"""{"bool": {"should": [
          {"match": { "customer_category": { "query": "Fun", "type": "phrase" }}},
          {"match": { "customer_category": { "query": "Times", "type": "phrase" }}}
         ], "minimum_should_match": "1" }
      }"""
      val actual = JsonReader.fromString(categoryQuery.get.toString)
      actual should be(expected)
    }
  }


  "DocumentQueries: domainTagsQuery" should {
    "return the expected query if no tags are given" in {
      val tagQuery = DocumentQueries.domainTagsQuery(None)
      tagQuery should be(None)
    }

    "return the expected query if one tag is given" in {
      val tagQuery = DocumentQueries.domainTagsQuery(Some(Set("Fun")))
      val expected = j"""{"bool": {"should":
          {"match": { "customer_tags": { "query": "Fun", "type": "phrase" }}},
         "minimum_should_match": "1" }
      }"""
      val actual = JsonReader.fromString(tagQuery.get.toString)
      actual should be(expected)
    }

    "return the expected query if multiple tags are given" in {
      val tagQuery = DocumentQueries.domainTagsQuery(Some(Set("Fun", "Times")))
      val expected = j"""{"bool": {"should": [
          {"match": { "customer_tags": { "query": "Fun", "type": "phrase" }}},
          {"match": { "customer_tags": { "query": "Times", "type": "phrase" }}}
         ], "minimum_should_match": "1" }
      }"""
      val actual = JsonReader.fromString(tagQuery.get.toString)
      actual should be(expected)
    }
  }

  "DocumentFilters: datatypeFilter" should {
    "return the expected filter if no datatypes are given" in {
      val datatypeFilter = DocumentFilters.datatypeFilter(None)
      datatypeFilter should be(None)
    }

    "return the expected filter if one datatype is given" in {
      val datatypeFilter = DocumentFilters.datatypeFilter(Some(Set("datasets")))
      val expected = j"""{ "terms": { "datatype": [ "dataset" ] } }"""
      val actual = JsonReader.fromString(datatypeFilter.get.toString)
      actual should be(expected)
    }

    "return the expected filter if multiple datatypes are given" in {
      val datatypeFilter = DocumentFilters.datatypeFilter(Some(Set("datasets", "datalens")))
      val expected = j"""{ "terms": { "datatype": [ "dataset", "datalens" ] } }"""
      val actual = JsonReader.fromString(datatypeFilter.get.toString)
      actual should be(expected)
    }

    "return the expected filter if a prefix is used" in {
      val datatypeFilter = DocumentFilters.datatypeFilter(Some(Set("pulses")), "new_")
      val expected = j"""{ "terms": { "new_datatype": [ "pulse" ] } }"""
      val actual = JsonReader.fromString(datatypeFilter.get.toString)
      actual should be(expected)
    }
  }

  "DocumentFilters: userFilter" should {
    "return the expected filter if no user is given" in {
      val userFilter = DocumentFilters.ownerFilter(None)
      userFilter should be(None)
    }

    "return the expected filter if some user is given" in {
      val userFilter = DocumentFilters.ownerFilter(Some("wonder-woman"))
      val expected = j"""{ "term": { "owner.id": "wonder-woman" } }"""
      val actual = JsonReader.fromString(userFilter.get.toString)
      actual should be(expected)
    }

    "return the expected filter if a prefix is used" in {
      val userFilter = DocumentFilters.ownerFilter(Some("wonder-woman"), "document.")
      val expected = j"""{ "term": { "document.owner.id": "wonder-woman" } }"""
      val actual = JsonReader.fromString(userFilter.get.toString)
      actual should be(expected)
    }
  }

  "DocumentFilters: domainIdsFilter" should {
    "return the expected filter if one id is given" in {
      val domainIdFilter = DocumentFilters.domainIdsFilter(Set(1))
      val expected = j"""{ "terms": { "socrata_id.domain_id": [ 1 ] } }"""
      val actual = JsonReader.fromString(domainIdFilter.toString)
      actual should be(expected)
    }

    "return the expected filter if multiple ids are given" in {
      val domainIdFilter = DocumentFilters.domainIdsFilter(Set(1, 42))
      val expected = j"""{ "terms": { "socrata_id.domain_id": [ 1, 42 ] } }"""
      val actual = JsonReader.fromString(domainIdFilter.toString)
      actual should be(expected)
    }

    "return the expected filter if a prefix is used" in {
      val domainIdFilter = DocumentFilters.domainIdsFilter(Set(1, 42), "demo.")
      val expected = j"""{ "terms": { "demo.socrata_id.domain_id": [ 1, 42 ] } }"""
      val actual = JsonReader.fromString(domainIdFilter.toString)
      actual should be(expected)
    }
  }

  "DocumentFilters: isApprovedByParentDomainFilter" should {
    "return the expected filter when no prefix is given" in {
      val approvalFilter = DocumentFilters.isApprovedByParentDomainFilter()
      val expected = j"""{ "term": { "is_approved_by_parent_domain": true } }"""
      val actual = JsonReader.fromString(approvalFilter.toString)
      actual should be(expected)
    }

    "return the expected filter when some prefix is given" in {
      val approvalFilter = DocumentFilters.isApprovedByParentDomainFilter("child_")
      val expected = j"""{ "term": { "child_is_approved_by_parent_domain": true } }"""
      val actual = JsonReader.fromString(approvalFilter.toString)
      actual should be(expected)
    }
  }

  "DocumentFilters: domainMetadataFilter" should {
    "return the expected filter when no metadata is given" in {
      val filter = DocumentFilters.domainMetadataFilter(None)
      filter should be(None)
    }

    "return the expected filter when an empty set of metadata is given" in {
      val filter = DocumentFilters.domainMetadataFilter(Some(Set.empty[(String, String)]))
      filter should be(None)
    }

    "return the expected filter when a metadata query lists a single key, single value pair" in {
      val filter = DocumentFilters.domainMetadataFilter(Some(Set(("org", "ny"))))
      val expected =
        j"""{"bool": {"should": {"nested": {"filter": {"bool": {"must": [
            {"terms": {"customer_metadata_flattened.key.raw": [ "org" ] }},
            {"terms": {"customer_metadata_flattened.value.raw": [ "ny" ] }}
          ]}}, "path": "customer_metadata_flattened" }}}
      }"""
      val actual = JsonReader.fromString(filter.get.toString)
      actual should be(expected)
    }

    "return the expected filter when a metadata query lists a single key but multiple values" in {
      val filter = DocumentFilters.domainMetadataFilter(Some(Set(("org", "ny"), ("org", "nj"))))
      val expected =
        j"""{"bool": {"should": [
            {"nested": {"filter": {"bool": {"must": [
              {"terms": {"customer_metadata_flattened.key.raw": [ "org" ] }},
              {"terms": {"customer_metadata_flattened.value.raw": [ "ny" ] }}
            ]}}, "path": "customer_metadata_flattened" }},
            {"nested": {"filter": {"bool": {"must": [
              {"terms": {"customer_metadata_flattened.key.raw": [ "org" ] }},
              {"terms": {"customer_metadata_flattened.value.raw": [ "nj" ] }}
            ]}}, "path": "customer_metadata_flattened" }}]}
      }"""
      val actual = JsonReader.fromString(filter.get.toString)
      actual should be(expected)
    }

    "return the expected filter when a metadata query lists multiple sets of single key/single value pairs" in {
      val filter = DocumentFilters.domainMetadataFilter(Some(Set(("org", "chicago"), ("owner", "john"))))
      val expected =
        j"""{"bool": {"must": [
               {"bool": {"should": {"nested": {"filter": {"bool": {"must": [
                 {"terms": {"customer_metadata_flattened.key.raw": [ "org" ] }},
                 {"terms": {"customer_metadata_flattened.value.raw": [ "chicago" ] }}
               ]}}, "path": "customer_metadata_flattened" }}}},
               {"bool": {"should": {"nested": {"filter": {"bool": {"must": [
                 {"terms": {"customer_metadata_flattened.key.raw": [ "owner" ] }},
                 {"terms": {"customer_metadata_flattened.value.raw": [ "john" ] }}
               ]}}, "path": "customer_metadata_flattened" }}}}
             ]}
      }"""
      val actual = JsonReader.fromString(filter.get.toString)
      actual should be(expected)
    }

    "return the expected filter when a metadata query lists multiple sets of keys with multiple values" in {
      val filter = DocumentFilters.domainMetadataFilter(Some(Set(("org", "chicago"), ("owner", "john"), ("org", "ny"))))
      val expected =
        j"""{"bool": {"must": [
               {"bool": {"should": [
                  {"nested": {"filter": {"bool": {"must": [
                    {"terms": {"customer_metadata_flattened.key.raw": [ "org" ] }},
                    {"terms": {"customer_metadata_flattened.value.raw": [ "chicago" ] }}
                  ]}}, "path": "customer_metadata_flattened" }},
                  {"nested": {"filter": {"bool": {"must": [
                    {"terms": {"customer_metadata_flattened.key.raw": [ "org" ] }},
                    {"terms": {"customer_metadata_flattened.value.raw": [ "ny" ] }}
                  ]}}, "path": "customer_metadata_flattened" }}]}
               },
               {"bool": {"should": {"nested": {"filter": {"bool": {"must": [
                 {"terms": {"customer_metadata_flattened.key.raw": [ "owner" ] }},
                 {"terms": {"customer_metadata_flattened.value.raw": [ "john" ] }}
               ]}}, "path": "customer_metadata_flattened" }}}}
             ]}
        }"""
      val actual = JsonReader.fromString(filter.get.toString)
      actual should be(expected)
    }
  }

  "DocumentFilters: moderationStatusFilter" should {
    "have tests" in {

    }
  }

  "DocumentFilters: routingApprovalFilter" should {
    "have tests" in {

    }
  }

  "DocumentFilters: publicFilter" should {
    "return the expected filter" in {
      val filter = DocumentFilters.publicFilter(false)
      val actual = JsonReader.fromString(filter.toString)
      val expected =
        j"""{
        "not" : {
          "filter" : {
            "term" : {
              "is_public" : false
            }
          }
        }
      }"""
      actual should be(expected)
    }
  }

  "DocumentFilters: publishedFilter" should {
    "return the expected filter" in {
      val filter = DocumentFilters.publishedFilter(false)
      val actual = JsonReader.fromString(filter.toString)
      val expected =
        j"""{
        "not" : {
          "filter" : {
            "term" : {
              "is_published" : false
            }
          }
        }
      }"""
      actual should be(expected)
    }
  }
}
