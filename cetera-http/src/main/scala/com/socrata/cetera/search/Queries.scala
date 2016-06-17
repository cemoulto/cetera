package com.socrata.cetera.search

import com.socrata.cetera._
import com.socrata.cetera.handlers.{RelevanceParamSet, SearchParamSet}
import com.socrata.cetera.types.{DomainCategoryFieldType, DomainTagsFieldType, _}
import org.elasticsearch.index.query.MatchQueryBuilder.Type._
import org.elasticsearch.index.query.QueryBuilders._
import org.elasticsearch.index.query.{BaseQueryBuilder, MultiMatchQueryBuilder, QueryBuilder, QueryBuilders}

object DocumentQueries {
  private def applyDefaultTitleBoost(
    defaultTitleBoost: Option[Float],
    defaultMinShouldMatch: Option[String],
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
    defaultMinShouldMatch: Option[String],
    searchContext: Option[String])
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

  def generateMatchQuery(
    searchFilterSet: SearchParamSet,
    relevanceFilterSet: Option[RelevanceParamSet],
    defaultTitleBoost: Option[Float],
    defaultMinShouldMatch: Option[String]
  ) = {
    // Construct basic match query
    (searchFilterSet.searchQuery, relevanceFilterSet) match {
      case (NoQuery, _) => QueryBuilders.matchAllQuery
      // TODO: the 'get's in here are bad - refactor this!
      case (AdvancedQuery(queryString), Some(r)) => generateAdvancedQuery(queryString, r.fieldBoosts)
      case (SimpleQuery(queryString), Some(r)) => generateSimpleQuery(queryString,
        applyDefaultTitleBoost(defaultTitleBoost, defaultMinShouldMatch, r.fieldBoosts),
        r.datatypeBoosts,
        chooseMinShouldMatch(r.minShouldMatch, defaultMinShouldMatch, searchFilterSet.searchContext),
        r.slop)
      case (_, _) => ???
    }
  }

  def categoriesQuery(categories: Option[Set[String]]): Option[QueryBuilder] =
    categories.map { cs =>
      nestedQuery(
        CategoriesFieldType.fieldName,
        cs.foldLeft(boolQuery().minimumNumberShouldMatch(1)) { (b, q) =>
          b.should(matchQuery(CategoriesFieldType.Name.fieldName, q).`type`(PHRASE))
        }
      )
    }

  def tagsQuery(tags: Option[Set[String]]): Option[QueryBuilder] =
    tags.map { tags =>
      nestedQuery(
        TagsFieldType.fieldName,
        tags.foldLeft(boolQuery().minimumNumberShouldMatch(1)) { (b, q) =>
          b.should(matchQuery(TagsFieldType.Name.fieldName, q).`type`(PHRASE))
        }
      )
    }

  def domainCategoriesQuery(categories: Option[Set[String]]): Option[QueryBuilder] =
    categories.map { cs =>
      cs.foldLeft(boolQuery().minimumNumberShouldMatch(1)) { (b, q) =>
        b.should(matchQuery(DomainCategoryFieldType.fieldName, q).`type`(PHRASE))
      }
    }

  def domainTagsQuery(tags: Option[Set[String]]): Option[QueryBuilder] =
    tags.map { ts =>
      ts.foldLeft(boolQuery().minimumNumberShouldMatch(1)) { (b, q) =>
        b.should(matchQuery(DomainTagsFieldType.fieldName, q).`type`(PHRASE))
      }
    }
}

