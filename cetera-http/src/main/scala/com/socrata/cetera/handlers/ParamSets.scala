package com.socrata.cetera.handlers

import com.socrata.cetera.types._

case class SearchParamSet(
  searchQuery: QueryType,
  domains: Option[Set[String]],
  searchContext: Option[String],
  domainMetadata: Option[Set[(String, String)]],
  categories: Option[Set[String]],
  tags: Option[Set[String]],
  datatypes: Option[Set[String]],
  user: Option[String],
  attribution: Option[String],
  parentDatasetId: Option[String])

//TODO  : replace searchContext with domain_cataegories and domain_tags.
case class VisibilityParamSet(
  viewApproval: Option[String],   // approved, pending, rejected
  datasetApproval: Option[String],
  datalensApproval: Option[String])

case class RelevanceParamSet(
  fieldBoosts: Map[CeteraFieldType with Boostable, Float],
  datatypeBoosts: Map[Datatype, Float],
  domainBoosts: Map[String, Float],
  minShouldMatch: Option[String],
  slop: Option[Int],
  showScore: Boolean)

case class PagingParamSet(
  offset: Int,
  limit: Int,
  sortOrder: Option[String])
