package com.socrata.cetera.search

import org.elasticsearch.index.query.FilterBuilders
import org.elasticsearch.search.sort.{FieldSortBuilder, SortBuilder, SortBuilders, SortOrder}

import com.socrata.cetera.types._

// TODO: Ultimately, these should accept Sortables rather than Strings
object Sorts {
  val sortScoreDesc: SortBuilder = {
    SortBuilders.scoreSort().order(SortOrder.DESC).missing("_last")
  }

  def sortFieldAsc(field: String): SortBuilder = {
    SortBuilders.fieldSort(field).order(SortOrder.ASC).missing("_last")
  }

  def sortFieldDesc(field: String): SortBuilder = {
    SortBuilders.fieldSort(field).order(SortOrder.DESC).missing("_last")
  }

  def buildAverageScoreSort(
      fieldName: String,
      rawFieldName: String,
      classifications: Set[String])
    : FieldSortBuilder = {

    SortBuilders
      .fieldSort(fieldName)
      .order(SortOrder.DESC)
      .missing("_last")
      .sortMode("avg")
      .setNestedFilter(
        FilterBuilders.termsFilter(
          rawFieldName,
          classifications.toSeq: _*
        )
      )
  }

  // Map of param to sorts on ES fields
  // These sorts are tentative and will not be documented in apiary until finalized
  val paramSortMap = Map[String, SortBuilder](
    // Default
    // Due to current logic in DocumentClient, this is used only as a key--its value is not returned from here.
    "relevance" -> sortScoreDesc,

    // Timestamps
    "createdAt ASC" -> sortFieldAsc(CreatedAtFieldType.fieldName),
    "createdAt DESC" -> sortFieldDesc(CreatedAtFieldType.fieldName),
    "createdAt" -> sortFieldDesc(CreatedAtFieldType.fieldName),

    "updatedAt ASC" -> sortFieldAsc(UpdatedAtFieldType.fieldName),
    "updatedAt DESC" -> sortFieldDesc(UpdatedAtFieldType.fieldName),
    "updatedAt" -> sortFieldDesc(UpdatedAtFieldType.fieldName),

    // Page views
    "page_views_last_week ASC" -> sortFieldAsc(PageViewsLastWeekFieldType.fieldName),
    "page_views_last_week DESC" -> sortFieldDesc(PageViewsLastWeekFieldType.fieldName),
    "page_views_last_week" -> sortFieldDesc(PageViewsLastWeekFieldType.fieldName),

    "page_views_last_month ASC" -> sortFieldAsc(PageViewsLastMonthFieldType.fieldName),
    "page_views_last_month DESC" -> sortFieldDesc(PageViewsLastMonthFieldType.fieldName),
    "page_views_last_month" -> sortFieldDesc(PageViewsLastMonthFieldType.fieldName),

    "page_views_total ASC" -> sortFieldAsc(PageViewsTotalFieldType.fieldName),
    "page_views_total DESC" -> sortFieldDesc(PageViewsTotalFieldType.fieldName),
    "page_views_total" -> sortFieldDesc(PageViewsTotalFieldType.fieldName),

    // Alphabetical
    "name" -> sortFieldAsc(NameFieldType.fieldName),
    "name ASC" -> sortFieldAsc(NameFieldType.fieldName),
    "name DESC" -> sortFieldDesc(NameFieldType.fieldName)
  )

  // First pass logic is very simple. query >> categories >> tags >> default
  def chooseSort(
      searchQuery: QueryType,
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]])
    : SortBuilder = {

    (searchQuery, searchContext, categories, tags) match {
      // ODN Categories
      case (NoQuery, None, Some(cats), _) =>
        buildAverageScoreSort(
          CategoriesFieldType.Score.fieldName,
          CategoriesFieldType.Name.rawFieldName,
          cats
        )

      // ODN Tags
      case (NoQuery, None, None, Some(ts)) =>
        buildAverageScoreSort(
          TagsFieldType.Score.fieldName,
          TagsFieldType.Name.rawFieldName,
          ts
        )

      // Everything else
      case _ => Sorts.sortScoreDesc
    }
  }
}
