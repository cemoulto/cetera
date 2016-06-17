package com.socrata.cetera.search

import org.elasticsearch.index.query.FilterBuilders._
import org.elasticsearch.index.query.MatchQueryBuilder.Type.PHRASE
import org.elasticsearch.index.query.QueryBuilders._
import org.elasticsearch.index.query.{FilterBuilder, QueryBuilder, QueryBuilders}
import com.socrata.cetera._
import com.socrata.cetera.types._

object DocumentFilters {
  def datatypeFilter(datatypes: Option[Set[String]], aggPrefix: String = ""): Option[FilterBuilder] =
    datatypes.map(ts => datatypeFilter(ts, aggPrefix))

  def datatypeFilter(datatypes: Set[String], aggPrefix: String): FilterBuilder = {
    val validatedDatatypes = datatypes.flatMap(t => Datatype(t).map(_.singular))
    termsFilter(aggPrefix + DatatypeFieldType.fieldName, validatedDatatypes.toSeq: _*)
  }

  def userFilter(user: Option[String], aggPrefix: String = ""): Option[FilterBuilder] =
    user.map(userFilter(_, aggPrefix))

  def userFilter(user: String, aggPrefix: String): FilterBuilder =
    termFilter(aggPrefix + OwnerIdFieldType.fieldName, user)

  def attributionFilter(attribution: String, aggPrefix: String): FilterBuilder =
    termFilter(aggPrefix + AttributionFieldType.rawFieldName, attribution)

  def attributionFilter(attribution: Option[String], aggPrefix: String = ""): Option[FilterBuilder] =
    attribution.map(attributionFilter(_, aggPrefix: String))

  def parentDatasetFilter(parentDatasetId: Option[String], aggPrefix: String = ""): Option[FilterBuilder] =
    parentDatasetId.map(parentDatasetFilter(_, aggPrefix))

  def parentDatasetFilter(parentDatasetId: String, aggPrefix: String): FilterBuilder =
    termFilter(aggPrefix + ParentDatasetIdFieldType.fieldName, parentDatasetId)

  // Don't call me unless you actually want to build a filter
  def domainIdsFilter(domainIds: Set[Int], aggPrefix: String = ""): FilterBuilder =
    termsFilter(aggPrefix + SocrataIdDomainIdFieldType.fieldName, domainIds.toSeq: _*)

  def isApprovedByParentDomainFilter(aggPrefix: String = ""): FilterBuilder =
    termFilter(aggPrefix + "is_approved_by_parent_domain", true)

  // TODO:  should we score metadata by making this a query?
  // and if you do, remember to make the key and value both phrase matches
  def domainMetadataFilter(metadata: Option[Set[(String, String)]]): Option[FilterBuilder] =
    metadata.flatMap {
      case metadataKeyValues: Set[(String, String)] if metadataKeyValues.nonEmpty =>
        val metadataGroupedByKey = metadataKeyValues
          .groupBy { case (k, v) => k }
          .map { case (key, set) => key -> set.map(_._2) }
        val unionWithinKeys = metadataGroupedByKey.map { case (k, vs) =>
          vs.foldLeft(boolFilter()) { (b, v) =>
            b.should(nestedFilter(
              DomainMetadataFieldType.fieldName,
              boolFilter()
                .must(termsFilter(DomainMetadataFieldType.Key.rawFieldName, k))
                .must(termsFilter(DomainMetadataFieldType.Value.rawFieldName, v))
            ))
          }
        }

        if (metadataGroupedByKey.size == 1) {
          unionWithinKeys.headOption // no need to create an intersection for 1 key
        } else {
          val intersectAcrossKeys = unionWithinKeys.foldLeft(boolFilter()) { (b, q) => b.must(q) }
          Some(intersectAcrossKeys)
        }
      case _ => None  // no filter to build from the empty set
    }

  /**
    * Filter to only show documents that have passed the view moderation workflow.
    *
    * in general a document is visible when either:
    *   it is a default view,
    *   moderation is approved,
    *   parent domain moderation is disabled.
    * datalens has a special treatment: **LENS**
    *   it is a default view, (which may never occur) or
    *   moderation is approved,
    *   we usually show more things when parent domain moderation disabled, but not datalenses!
    * federation is mostly the same but has one special treatment: **FED**
    *   when search context is moderated, and parent domain is not moderated: only show defaults!
    *
    * @param searchContextIsModerated is the catalog search context view moderated?
    * @return composable filter builder
    */
  def moderationStatusFilter(searchContextIsModerated: Boolean,
                             moderatedDomainIds: Set[Int],
                             unmoderatedDomainIds: Set[Int],
                             isDomainAgg: Boolean = false): FilterBuilder = {
    val aggPrefix = if (isDomainAgg) esDocumentType + "." else ""
    val documentIsDefault = termFilter(aggPrefix + IsDefaultViewFieldType.fieldName, true)
    val documentIsAccepted = termFilter(aggPrefix + IsModerationApprovedFieldType.fieldName, true)

    val parentDomainIsModerated =
      Some(moderatedDomainIds).filter(_.nonEmpty).map(mdids => domainIdsFilter(mdids, aggPrefix))

    val parentDomainIsNotModerated =
      Some(unmoderatedDomainIds).filter(_.nonEmpty).map(udids => domainIdsFilter(udids, aggPrefix))

    val datalensUniqueAndSpecialSnowflakeFilter = datatypeFilter(
      Set(TypeDatalenses.singular, TypeDatalensCharts.singular, TypeDatalensMaps.singular),
      aggPrefix
    )
    val documentIsNotDatalens = notFilter(datalensUniqueAndSpecialSnowflakeFilter)

    val basicFilter = boolFilter()
      .should(documentIsDefault)
      .should(documentIsAccepted)

    parentDomainIsNotModerated.foreach(f => basicFilter.should(
      boolFilter()
        .must(f)
        .must(documentIsNotDatalens)
    ))

    val contextualFilter = boolFilter()
    parentDomainIsModerated.foreach(f => contextualFilter.should(
      boolFilter()
        .must(f)
        .must(boolFilter()
          .should(documentIsDefault)
          .should(documentIsAccepted)
        )
    ))
    parentDomainIsNotModerated.foreach(f => contextualFilter.should(
      boolFilter()
        .must(f)
        .must(documentIsDefault)
    ))

    if (searchContextIsModerated) contextualFilter else basicFilter
  }

  /**
    * Filter to only show or aggregate documents that have passed the routing & approval workflow
    *
    * in general a document is visible when either:
    *   parent domain R&A is disabled, or
    *   document is approved by parent domain.
    * federation additionally requires that either:
    *   search context domain R&A is disabled, or
    *   document is approved by search context domain.
    *
    * @param searchContext the catalog search context
    * @param isDomainAgg is the search an aggregation on domains
    * @return composable filter builder
    */
  def routingApprovalFilter(searchContext: Option[Domain],
                            raOffDomainIds: Set[Int],
                            isDomainAgg: Boolean = false): FilterBuilder = {
    val prefix = if (isDomainAgg) esDocumentType + "." else ""

    val documentIsApprovedBySearchContext = searchContext.flatMap { d =>
      if (d.routingApprovalEnabled) {
        Some(termsFilter(ApprovingDomainIdsFieldType.fieldName, d.domainId))
      } else { None }
    }

    val documentRAVisible = boolFilter()

    if (raOffDomainIds.nonEmpty) documentRAVisible.should(domainIdsFilter(raOffDomainIds, prefix))
    documentRAVisible.should(isApprovedByParentDomainFilter(prefix))

    val filter = boolFilter()
    documentIsApprovedBySearchContext.foreach(filter.must)
    filter.must(documentRAVisible)
    filter
  }

  def publicFilter(isDomainAgg: Boolean = false): FilterBuilder = {
    val prefix = if (isDomainAgg) esDocumentType + "." else ""
    notFilter(termFilter(prefix + IsPublicFieldType.fieldName, false))
  }

  def publishedFilter(isDomainAgg: Boolean = false): FilterBuilder = {
    val prefix = if (isDomainAgg) esDocumentType + "." else ""
    notFilter(termFilter(prefix + IsPublishedFieldType.fieldName, false))
  }
}

object DomainFilters {
  def domainIdsFilter(domainIds: Set[Int]): FilterBuilder = termsFilter("domain_id", domainIds.toSeq: _*)

  // two nos make a yes: this filters out items with is_customer_domain=false, while permitting true or null.
  def isNotCustomerDomainFilter: FilterBuilder = termFilter(IsCustomerDomainFieldType.fieldName, false)
  def isCustomerDomainFilter: FilterBuilder = notFilter(isNotCustomerDomainFilter)
}
