package com.socrata.cetera.search

import org.elasticsearch.action.search.SearchRequestBuilder
import org.elasticsearch.index.query._
import org.elasticsearch.search.aggregations.AggregationBuilders

import com.socrata.cetera._
import com.socrata.cetera.search.DocumentAggregations._
import com.socrata.cetera.handlers._
import com.socrata.cetera.types._

trait BaseDocumentClient {
  def buildSearchRequest(
      searchFilterSet: SearchParamSet,
      visibilityFilterSet: VisibilityParamSet,
      relevanceFilterSet: RelevanceParamSet,
      pagingFilterSet: PagingParamSet,
      domainSet: DomainSet)
    : SearchRequestBuilder

  def buildCountRequest(
      field: DocumentFieldType with Countable with Rawable,
      searchFilterSet: SearchParamSet,
      visibilityFilterSet: VisibilityParamSet,
      domainSet: DomainSet)
    : SearchRequestBuilder

  def buildFacetRequest(
    domain: Domain,
    searchFilterSet: SearchParamSet,
    visibilityParamSet: VisibilityParamSet): SearchRequestBuilder
}

class DocumentClient(
    esClient: ElasticSearchClient,
    domainClient: BaseDomainClient,
    indexAliasName: String,
    defaultTitleBoost: Option[Float],
    defaultMinShouldMatch: Option[String],
    scriptScoreFunctions: Set[ScriptScoreFunction])
  extends BaseDocumentClient {

  private def buildSearchParamsFilter(searchFilterSet: SearchParamSet): List[FilterBuilder] = {
    import com.socrata.cetera.search.DocumentFilters._ // scalastyle:ignore
    List.concat(
      datatypeFilter(searchFilterSet.datatypes),
      userFilter(searchFilterSet.user),
      attributionFilter(searchFilterSet.attribution),
      parentDatasetFilter(searchFilterSet.parentDatasetId),
      searchFilterSet.searchContext.flatMap(_ => domainMetadataFilter(searchFilterSet.domainMetadata)) // I make it hard to de-option
    )
  }

  private def buildVisibilityParamsFilter(visibilityParamSet: VisibilityParamSet,
    domainSet: DomainSet):  List[FilterBuilder]  = {

    import com.socrata.cetera.search.DocumentFilters._ // scalastyle:ignore

    val contextModerated = domainSet.searchContext.exists(_.moderationEnabled)

    val (domainIds,
    moderatedDomainIds,
    unmoderatedDomainIds,
    routingApprovalDisabledDomainIds) = domainSet.calculateIdsAndModRAStatuses

    val domainFilter = domainIdsFilter(domainIds)
    List.concat(
      Some(publicFilter()),
      Some(publishedFilter()),
      Some(moderationStatusFilter(contextModerated, moderatedDomainIds, unmoderatedDomainIds)),
      Some(routingApprovalFilter(domainSet.searchContext, routingApprovalDisabledDomainIds))
    )
  }

  private def buildCompositeFilter(
    searchFilterSet: SearchParamSet,
    visibilityParamSet: VisibilityParamSet,
    domainSet: DomainSet): FilterBuilder = {

    val searchParamFilters = buildSearchParamsFilter(searchFilterSet)
    val visibilityParamFilters = buildVisibilityParamsFilter(visibilityParamSet, domainSet)

    val filter = FilterBuilders.boolFilter()
    val allFilters = searchParamFilters ++ visibilityParamFilters
    allFilters.foreach(filter.must)
    filter
  }

  // scalastyle:ignore parameter.number
  private def buildFilteredQuery(query: BaseQueryBuilder,
    searchFilterSet: SearchParamSet,
    visibilityParamSet: VisibilityParamSet,
    domainSet: DomainSet
    ): BaseQueryBuilder = {

    import com.socrata.cetera.search.DocumentQueries._ // scalastyle:ignore

    // If there is no search context, use the ODN categories and tags
    // otherwise use the custom domain categories and tags
    val categoriesAndTags: Seq[QueryBuilder] =
      if (searchFilterSet.searchContext.isDefined) {
        List.concat(
          domainCategoriesQuery(searchFilterSet.categories),
          domainTagsQuery(searchFilterSet.tags))
      } else {
        List.concat(
          categoriesQuery(searchFilterSet.categories),
          tagsQuery(searchFilterSet.tags))
      }

    val categoriesAndTagsQuery =
      if (categoriesAndTags.nonEmpty) {
        categoriesAndTags.foldLeft(QueryBuilders.boolQuery().must(query)) { (b, q) => b.must(q) }
      } else { query }

    // This is a FilterBuilder, which incorporates all of the remaining constraints.
    // These constraints determine whether a document is considered part of the selection set, but
    // they do not affect the relevance score of the document.
    val compositeFilter = buildCompositeFilter(searchFilterSet, visibilityParamSet, domainSet)

    QueryBuilders.filteredQuery(categoriesAndTagsQuery, compositeFilter)
  }


  // Assumes validation has already been done
  //
  // Called by buildSearchRequest and buildCountRequest
  //
  // * Chooses query type to be used and constructs query with applicable boosts
  // * Applies function scores (typically views and score) with applicable domain boosts
  // * Applies filters (facets and searchContext-sensitive federation preferences)
  def buildBaseRequest(
      searchFilterSet: SearchParamSet,
      visibilityFilterSet: VisibilityParamSet,
      relevanceFilterSet: Option[RelevanceParamSet],
      pagingFilterSet: Option[PagingParamSet],
      domainSet: DomainSet)
    : SearchRequestBuilder = {

    // Construct basic match query
    val matchQuery = DocumentQueries.generateMatchQuery(searchFilterSet, relevanceFilterSet,
      defaultTitleBoost, defaultMinShouldMatch)

    // Wrap basic match query in filtered query for filtering
    val filteredQuery = buildFilteredQuery(matchQuery, searchFilterSet, visibilityFilterSet, domainSet)

    // Wrap filtered query in function score query for boosting
    val query = QueryBuilders.functionScoreQuery(filteredQuery)
    Boosts.applyScoreFunctions(query, scriptScoreFunctions)
    relevanceFilterSet.foreach{ r => Boosts.applyDomainBoosts(query, domainSet.domainIdBoosts(r.domainBoosts))}
    query.scoreMode("multiply").boostMode("replace")

    val preparedSearch = esClient.client
      .prepareSearch(indexAliasName)
      .setQuery(query)
      .setTypes(esDocumentType)

    preparedSearch
  }

  def buildSearchRequest(
    searchFilterSet: SearchParamSet,
    visibilityFilterSet: VisibilityParamSet,
    relevanceFilterSet: RelevanceParamSet,
    pagingFilterSet: PagingParamSet,
    domainSet: DomainSet)
  : SearchRequestBuilder = {

    val baseRequest = buildBaseRequest(
      searchFilterSet,
      visibilityFilterSet,
      Some(relevanceFilterSet),
      Some(pagingFilterSet),
      domainSet)

    // WARN: Sort will totally blow away score if score isn't part of the sort
    // "Relevance" without a query can mean different things, so chooseSort decides
    val sort = pagingFilterSet.sortOrder match {
      case Some(so) if so != "relevance" => Sorts.paramSortMap.get(so).get // will raise if invalid param got through
      case _ => Sorts.chooseSort(searchFilterSet)
    }

    baseRequest
      .setFrom(pagingFilterSet.offset)
      .setSize(pagingFilterSet.limit)
      .addSort(sort)
  }

  def buildCountRequest(
    field: DocumentFieldType with Countable with Rawable,
    searchFilterSet: SearchParamSet,
    visibilityFilterSet: VisibilityParamSet,
    domainSet: DomainSet)
  : SearchRequestBuilder = {

    val aggregation = chooseAggregation(field)

    val baseRequest = buildBaseRequest(
      searchFilterSet,
      visibilityFilterSet,
      None, // no relevance filters
      None, // no paging filters
      domainSet
    )

    baseRequest
      .addAggregation(aggregation)
      .setSearchType("count")
      .setSize(0) // no docs, aggs only
  }

  def buildFacetRequest(domain: Domain,
    searchFilterSet: SearchParamSet,
    visibilityParamSet: VisibilityParamSet): SearchRequestBuilder = {
    val aggSize = 0 // agg count unlimited
    val searchSize = 0 // no docs, aggs only

    val datatypeAgg = AggregationBuilders
      .terms("datatypes")
      .field(DatatypeFieldType.fieldName)
      .size(aggSize)

    val categoryAgg = AggregationBuilders
      .terms("categories")
      .field(DomainCategoryFieldType.rawFieldName)
      .size(aggSize)

    val tagAgg = AggregationBuilders
      .terms("tags")
      .field(DomainTagsFieldType.rawFieldName)
      .size(aggSize)

    val metadataAgg = AggregationBuilders
      .nested("metadata")
      .path(DomainMetadataFieldType.fieldName)
      .subAggregation(AggregationBuilders.terms("keys")
        .field(DomainMetadataFieldType.Key.rawFieldName)
        .size(aggSize)
        .subAggregation(AggregationBuilders.terms("values")
          .field(DomainMetadataFieldType.Value.rawFieldName)
          .size(aggSize)))

    val domainSpecificFilter = buildCompositeFilter(searchFilterSet, visibilityParamSet, DomainSet(Set(domain), None))

    val filteredAggs = AggregationBuilders
      .filter("domain_filter")
      .filter(domainSpecificFilter)
      .subAggregation(datatypeAgg)
      .subAggregation(categoryAgg)
      .subAggregation(tagAgg)
      .subAggregation(metadataAgg)

    val preparedSearch = esClient.client
      .prepareSearch(indexAliasName)
      .addAggregation(filteredAggs)
      .setSize(searchSize)

    preparedSearch
  }
}
