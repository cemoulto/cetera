package com.socrata.cetera.search

import org.elasticsearch.action.search.SearchRequestBuilder
import org.elasticsearch.index.query._
import org.elasticsearch.search.aggregations.AggregationBuilders

import com.socrata.cetera._
import com.socrata.cetera.search.DocumentAggregations._
import com.socrata.cetera.types._

trait BaseDocumentClient {
  def buildSearchRequest( // scalastyle:ignore parameter.number
      searchQuery: QueryType,
      domains: Set[Domain],
      domainMetadata: Option[Set[(String, String)]],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      datatypes: Option[Set[String]],
      user: Option[String],
      recipient: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String],
      fieldBoosts: Map[CeteraFieldType with Boostable, Float],
      datatypeBoosts: Map[Datatype, Float],
      domainIdBoosts: Map[Int, Float],
      minShouldMatch: Option[String],
      slop: Option[Int],
      offset: Int,
      limit: Int,
      sortOrder: Option[String])
    : SearchRequestBuilder

  def buildPersonalCatalogRequest( // scalastyle:ignore parameter.number
      user: Option[String],
      recipient: Option[String],
      searchQuery: QueryType,
      searchContext: Domain,
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      domainMetadata: Option[Set[(String, String)]],
      datatypes: Option[Set[String]],
      attribution: Option[String],
      parentDatasetId: Option[String],
      fieldBoosts: Map[CeteraFieldType with Boostable, Float],
      datatypeBoosts: Map[Datatype, Float],
      minShouldMatch: Option[String],
      slop: Option[Int],
      offset: Int,
      limit: Int,
      sortOrder: Option[String])
  : SearchRequestBuilder

  def buildCountRequest( // scalastyle:ignore parameter.number
      field: DocumentFieldType with Countable with Rawable,
      searchQuery: QueryType,
      domains: Set[Domain],
      domainMetadata: Option[Set[(String, String)]],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      datatypes: Option[Set[String]],
      user: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String])
    : SearchRequestBuilder

  def buildFacetRequest(domain: Option[Domain]): SearchRequestBuilder
}

class DocumentClient(
    esClient: ElasticSearchClient,
    domainClient: BaseDomainClient,
    indexAliasName: String,
    defaultTitleBoost: Option[Float],
    defaultMinShouldMatch: Option[String],
    scriptScoreFunctions: Set[ScriptScoreFunction])
  extends BaseDocumentClient {

  // This query is complex, as it generates two queries that are then combined
  // into a single query. By default, the must match clause enforces a term match
  // such that one or more of the query terms must be present in at least one of the
  // fields specified. The optional minimum_should_match constraint applies to this
  // clause. The should clause is intended to give a subset of retrieved results boosts
  // based on:
  //
  //   1. better phrase matching in the case of multiterm queries
  //   2. matches in particular fields (specified in fieldBoosts)
  //
  // The scores are then averaged together by ES with a defacto score of 0 for a should
  // clause if it does not in fact match any documents. See the ElasticSearch
  // documentation here:
  //
  //   https://www.elastic.co/guide/en/elasticsearch/guide/current/proximity-relevance.html
  def generateSimpleQuery(
      queryString: String,
      fieldBoosts: Map[CeteraFieldType with Boostable, Float],
      datatypeBoosts: Map[Datatype, Float],
      minShouldMatch: Option[String],
      slop: Option[Int])
    : BaseQueryBuilder = {

    // Query #1: terms (must)
    val matchTerms = SundryBuilders.multiMatch(queryString, MultiMatchQueryBuilder.Type.CROSS_FIELDS)

    // Apply minShouldMatch if present
    minShouldMatch.foreach(min => SundryBuilders.applyMinMatchConstraint(matchTerms, min))

    // Query #2: phrase (should)
    val matchPhrase = SundryBuilders.multiMatch(queryString, MultiMatchQueryBuilder.Type.PHRASE)

    // If no slop is specified, we do not set a default
    slop.foreach(SundryBuilders.applySlopParam(matchPhrase, _))

    // Add any optional field boosts to "should" match clause
    fieldBoosts.foreach { case (field, weight) =>
      matchPhrase.field(field.fieldName, weight)
    }

    // Combine the two queries above into a single Boolean query
    val query = QueryBuilders.boolQuery().must(matchTerms).should(matchPhrase)

    // Add datatype boosts (if any). These end up in the should clause.
    // NOTE: These boosts are normalized (i.e., not absolute weights on final scores).
    Boosts.applyDatatypeBoosts(query, datatypeBoosts)

    query
  }

  // NOTE: Advanced queries respect fieldBoosts but not datatypeBoosts
  // Q: Is this expected and desired?
  def generateAdvancedQuery(
      queryString: String,
      fieldBoosts: Map[CeteraFieldType with Boostable, Float])
    : BaseQueryBuilder = {

    val documentQuery = QueryBuilders
      .queryStringQuery(queryString)
      .field(FullTextSearchAnalyzedFieldType.fieldName)
      .field(FullTextSearchRawFieldType.fieldName)
      .autoGeneratePhraseQueries(true)

    val domainQuery = QueryBuilders
      .queryStringQuery(queryString)
      .field(FullTextSearchAnalyzedFieldType.fieldName)
      .field(FullTextSearchRawFieldType.fieldName)
      .field(DomainCnameFieldType.fieldName)
      .autoGeneratePhraseQueries(true)

    fieldBoosts.foreach { case (field, weight) =>
        documentQuery.field(field.fieldName, weight)
        domainQuery.field(field.fieldName, weight)
      }

    QueryBuilders.boolQuery()
      .should(documentQuery)
      .should(QueryBuilders.hasParentQuery(esDomainType, domainQuery))
  }

  private def buildCompositeFilter(
      domains: Set[Domain],
      datatypes: Option[Set[String]],
      user: Option[String],
      recipient: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String],
      searchContext: Option[Domain],
      domainMetadata: Option[Set[(String, String)]])
    : FilterBuilder = {

    import com.socrata.cetera.search.DocumentFilters._ // scalastyle:ignore

    val contextModerated = searchContext.exists(_.moderationEnabled)

    val (domainIds,
      moderatedDomainIds,
      unmoderatedDomainIds,
      routingApprovalDisabledDomainIds) = domainClient.calculateIdsAndModRAStatuses(domains)

    val domainFilter = domainIdsFilter(domainIds)

    val filter = FilterBuilders.boolFilter()
    List.concat(
      datatypeFilter(datatypes),
      userFilter(user),
      recipientFilter(recipient),
      attributionFilter(attribution),
      parentDatasetFilter(parentDatasetId),
      Some(domainFilter), // TODO: remove me since I am the superset!
      Some(publicFilter()),
      Some(publishedFilter()),
      Some(moderationStatusFilter(contextModerated, moderatedDomainIds, unmoderatedDomainIds)),
      Some(routingApprovalFilter(searchContext, routingApprovalDisabledDomainIds)),
      searchContext.flatMap(_ => domainMetadataFilter(domainMetadata)) // I make it hard to de-option
    ).foreach(filter.must)

    filter
  }

  // scalastyle:ignore parameter.number
  private def buildFilteredQuery(
      datatypes: Option[Set[String]],
      user: Option[String],
      recipient: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String],
      domains: Set[Domain],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      domainMetadata: Option[Set[(String, String)]],
      query: BaseQueryBuilder)
    : BaseQueryBuilder = {

    import com.socrata.cetera.search.DocumentQueries._ // scalastyle:ignore

    // If there is no search context, use the ODN categories and tags
    // otherwise use the custom domain categories and tags
    val categoriesAndTags: Seq[QueryBuilder] =
      if (searchContext.isDefined) {
        List.concat(
          domainCategoriesQuery(categories),
          domainTagsQuery(tags))
      } else {
        List.concat(
          categoriesQuery(categories),
          tagsQuery(tags))
      }

    val categoriesAndTagsQuery =
      if (categoriesAndTags.nonEmpty) {
        categoriesAndTags.foldLeft(QueryBuilders.boolQuery().must(query)) { (b, q) => b.must(q) }
      } else { query }

    // This is a FilterBuilder, which incorporates all of the remaining constraints.
    // These constraints determine whether a document is considered part of the selection set, but
    // they do not affect the relevance score of the document.
    val compositeFilter = buildCompositeFilter(
      domains, datatypes, user, recipient, attribution, parentDatasetId, searchContext, domainMetadata)

    QueryBuilders.filteredQuery(categoriesAndTagsQuery, compositeFilter)
  }

  private def applyDefaultTitleBoost(
      fieldBoosts: Map[CeteraFieldType with Boostable, Float])
    : Map[CeteraFieldType with Boostable, Float] = {

    // Look for default title boost; if a title boost is specified as a query
    // parameter, it will override the default
    defaultTitleBoost
      .map(boost => Map(TitleFieldType -> boost) ++ fieldBoosts)
      .getOrElse(fieldBoosts)
  }

  def chooseMinShouldMatch(
      minShouldMatch: Option[String],
      searchContext: Option[Domain])
    : Option[String] = {

    (minShouldMatch, searchContext) match {
      // If a minShouldMatch value is passed in, we must honor that.
      case (Some(msm), _) => minShouldMatch

      // If a minShouldMatch value is absent but a searchContext is present,
      // use default minShouldMatch settings for better precision.
      case (None, Some(sc)) => defaultMinShouldMatch

      // If neither is present, then do not use minShouldMatch.
      case (None, None) => None
    }
  }

  // Assumes validation has already been done
  //
  // Called by buildSearchRequest and buildCountRequest
  //
  // * Chooses query type to be used and constructs query with applicable boosts
  // * Applies function scores (typically views and score) with applicable domain boosts
  // * Applies filters (facets and searchContext-sensitive federation preferences)
  def buildBaseRequest( // scalastyle:ignore parameter.number method.length
      searchQuery: QueryType,
      domains: Set[Domain],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      domainMetadata: Option[Set[(String, String)]],
      datatypes: Option[Set[String]],
      user: Option[String],
      recipient: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String],
      fieldBoosts: Map[CeteraFieldType with Boostable, Float],
      datatypeBoosts: Map[Datatype, Float],
      domainIdBoosts: Map[Int, Float],
      minShouldMatch: Option[String],
      slop: Option[Int])
    : SearchRequestBuilder = {

    // Construct basic match query
    val matchQuery = searchQuery match {
      case NoQuery => QueryBuilders.matchAllQuery
      case AdvancedQuery(queryString) => generateAdvancedQuery(queryString, fieldBoosts)
      case SimpleQuery(queryString) => generateSimpleQuery(queryString,
        applyDefaultTitleBoost(fieldBoosts),
        datatypeBoosts,
        chooseMinShouldMatch(minShouldMatch, searchContext),
        slop)
    }

    // Wrap basic match query in filtered query for filtering
    val filteredQuery = buildFilteredQuery(
      datatypes,
      user,
      recipient,
      attribution,
      parentDatasetId,
      domains,
      searchContext,
      categories,
      tags,
      domainMetadata,
      matchQuery)

    // Wrap filtered query in function score query for boosting
    val query = QueryBuilders.functionScoreQuery(filteredQuery)
    Boosts.applyScoreFunctions(query, scriptScoreFunctions)
    Boosts.applyDomainBoosts(query, domainIdBoosts)
    query.scoreMode("multiply").boostMode("replace")

    val preparedSearch = esClient.client
      .prepareSearch(indexAliasName)
      .setQuery(query)
      .setTypes(esDocumentType)

    preparedSearch
  }

  // TODO: refactor this out into smaller pieces
  // scalastyle:ignore parameter.number method.length
  def buildPersonalCatalogRequest(user: Option[String],
                                  recipient: Option[String],
                                  searchQuery: QueryType,
                                  searchContext: Domain,
                                  categories: Option[Set[String]],
                                  tags: Option[Set[String]],
                                  domainMetadata: Option[Set[(String, String)]],
                                  datatypes: Option[Set[String]],
                                  attribution: Option[String],
                                  parentDatasetId: Option[String],
                                  fieldBoosts: Map[CeteraFieldType with Boostable, Float],
                                  datatypeBoosts: Map[Datatype, Float],
                                  minShouldMatch: Option[String],
                                  slop: Option[Int],
                                  offset: Int,
                                  limit: Int,
                                  sortOrder: Option[String])
  : SearchRequestBuilder = {
    val matchQuery = searchQuery match {
      case NoQuery => QueryBuilders.matchAllQuery
      case AdvancedQuery(queryString) => generateAdvancedQuery(queryString, fieldBoosts)
      case SimpleQuery(queryString) => generateSimpleQuery(queryString,
        applyDefaultTitleBoost(fieldBoosts),
        datatypeBoosts,
        chooseMinShouldMatch(minShouldMatch, Some(searchContext)),
        slop)
    }

    import com.socrata.cetera.search.DocumentQueries._ // scalastyle:ignore

    // If there is no search context, use the ODN categories and tags
    // otherwise use the custom domain categories and tags
    val categoriesAndTags: Seq[QueryBuilder] =
      List.concat(
          domainCategoriesQuery(categories),
          domainTagsQuery(tags))

    val categoriesAndTagsQuery =
      if (categoriesAndTags.nonEmpty) {
        categoriesAndTags.foldLeft(QueryBuilders.boolQuery().must(matchQuery)) { (b, q) => b.must(q) }
      } else { matchQuery }

    // This is a FilterBuilder, which incorporates all of the remaining constraints.
    // These constraints determine whether a document is considered part of the selection set, but
    // they do not affect the relevance score of the document.
    val compositeFilter = {
      import com.socrata.cetera.search.DocumentFilters._ // scalastyle:ignore

      val domainFilter = domainIdsFilter(Set(searchContext.domainId))

      val filter = FilterBuilders.boolFilter()
      List.concat(
        datatypeFilter(datatypes),
        userFilter(user),
        recipientFilter(recipient),
        attributionFilter(attribution),
        parentDatasetFilter(parentDatasetId),
        Some(domainFilter), // TODO: remove me since I am the superset!
        domainMetadataFilter(domainMetadata) // I make it hard to de-option
      ).foreach(filter.must)

      filter
    }

    QueryBuilders.filteredQuery(categoriesAndTagsQuery, compositeFilter)

    val query = QueryBuilders.functionScoreQuery(compositeFilter)
    Boosts.applyScoreFunctions(query, scriptScoreFunctions)
    query.scoreMode("multiply").boostMode("replace")

    val preparedSearch = esClient.client
      .prepareSearch(indexAliasName)
      .setQuery(query)
      .setTypes(esDocumentType)

    preparedSearch
  }

  def buildSearchRequest( // scalastyle:ignore parameter.number method.length
      searchQuery: QueryType,
      domains: Set[Domain],
      domainMetadata: Option[Set[(String, String)]],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      datatypes: Option[Set[String]],
      user: Option[String],
      recipient: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String],
      fieldBoosts: Map[CeteraFieldType with Boostable, Float],
      datatypeBoosts: Map[Datatype, Float],
      domainIdBoosts: Map[Int, Float],
      minShouldMatch: Option[String],
      slop: Option[Int],
      offset: Int,
      limit: Int,
      sortOrder: Option[String])
    : SearchRequestBuilder = {

    val baseRequest = buildBaseRequest(
      searchQuery,
      domains,
      searchContext,
      categories,
      tags,
      domainMetadata,
      datatypes,
      user,
      recipient,
      attribution,
      parentDatasetId,
      fieldBoosts,
      datatypeBoosts,
      domainIdBoosts,
      minShouldMatch,
      slop
    )
    // WARN: Sort will totally blow away score if score isn't part of the sort
    // "Relevance" without a query can mean different things, so chooseSort decides
    val sort = sortOrder match {
      case Some(so) if so != "relevance" => Sorts.paramSortMap.get(so).get // will raise if invalid param got through
      case _ => Sorts.chooseSort(searchQuery, searchContext, categories, tags)
    }

    baseRequest
      .setFrom(offset)
      .setSize(limit)
      .addSort(sort)
  }

  def buildCountRequest( // scalastyle:ignore parameter.number
      field: DocumentFieldType with Countable with Rawable,
      searchQuery: QueryType,
      domains: Set[Domain],
      domainMetadata: Option[Set[(String, String)]],
      searchContext: Option[Domain],
      categories: Option[Set[String]],
      tags: Option[Set[String]],
      datatypes: Option[Set[String]],
      user: Option[String],
      attribution: Option[String],
      parentDatasetId: Option[String])
    : SearchRequestBuilder = {

    val aggregation = chooseAggregation(field)

    val baseRequest = buildBaseRequest(
      searchQuery,
      domains,
      searchContext,
      categories,
      tags,
      domainMetadata,
      datatypes,
      user,
      None,
      attribution,
      parentDatasetId,
      Map.empty,
      Map.empty,
      Map.empty,
      None,
      None
    )

    baseRequest
      .addAggregation(aggregation)
      .setSearchType("count")
      .setSize(0) // no docs, aggs only
  }

  def buildFacetRequest(domain: Option[Domain]): SearchRequestBuilder = {
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

    val domainSpecificFilter = domain
      .map(d => buildCompositeFilter(Set(d), None, None, None, None, None, None, None))
      .getOrElse(FilterBuilders.matchAllFilter())

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
