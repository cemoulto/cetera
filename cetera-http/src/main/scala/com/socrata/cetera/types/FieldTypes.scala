package com.socrata.cetera.types

// scalastyle:ignore number.of.types

/////////////////////////////////
// Sealed Traits for Cetera Types

// A field in a document in ES
sealed trait CeteraFieldType {
  val fieldName: String
}

sealed trait DocumentFieldType extends CeteraFieldType
sealed trait DomainFieldType extends CeteraFieldType
sealed trait UserFieldType extends CeteraFieldType

// A field that can be boosted (i.e, fields like title or description that can
// have extra weight given to then when matching keyword queries).
sealed trait Boostable extends CeteraFieldType

// A field that we allow to be be counted (e.g., how many documents of a given tag do we have?)
sealed trait Countable extends CeteraFieldType

// A field that we allow to be sorted on
sealed trait Sortable extends CeteraFieldType

// Used for nested fields like arrays or json blobs
sealed trait NestedField extends CeteraFieldType {
  protected val path: String
  protected lazy val keyName: String = this
    .getClass
    .getName
    .toLowerCase
    .split("\\$")
    .lastOption
    .getOrElse(throw new NoSuchElementException)

  val fieldName: String = s"$path.$keyName"
}

// These fields have a numeric score associated with them. For example, a
// document's category or tag might have a score of 0.95 to indicate that we
// are fairly confident about said category or tag. This score might be used as
// a default sort order when filtering.
sealed trait Scorable extends CeteraFieldType with Countable {
  val Name: NestedField
  val Score: NestedField
}

// For fields that have an additional "raw" version stored in ES
sealed trait Rawable extends CeteraFieldType {
  lazy val rawFieldName: String = fieldName + ".raw"
}

// For fields that are natively "raw", i.e. "not_analyzed"
sealed trait NativelyRawable extends Rawable {
  override lazy val rawFieldName: String = fieldName
}

// For key-value things like custom metadata fields
sealed trait Mapable extends CeteraFieldType {
  val Key: NestedField
  val Value: NestedField
}

///////////////////
// Full Text Search
case object FullTextSearchAnalyzedFieldType extends CeteraFieldType {
  val fieldName: String = "fts_analyzed"
}
case object FullTextSearchRawFieldType extends CeteraFieldType {
  val fieldName: String = "fts_raw"
}
case object DomainCnameFieldType extends DomainFieldType with Countable with Rawable {
  val fieldName: String = "domain_cname"
}

////////////////////
// Categories & Tags

// Categories have a score and a raw field name available
case object CategoriesFieldType extends DocumentFieldType with Scorable with Rawable {
  val fieldName: String = "animl_annotations.categories"

  case object Name extends NestedField with Rawable {
    protected lazy val path: String = CategoriesFieldType.fieldName
  }

  case object Score extends NestedField with Rawable {
    protected lazy val path: String = CategoriesFieldType.fieldName
  }
}

// Tags have a score and a raw field name available
case object TagsFieldType extends DocumentFieldType with Scorable with Rawable {
  val fieldName: String = "animl_annotations.tags"

  case object Name extends NestedField with Rawable {
    protected lazy val path: String = TagsFieldType.fieldName
  }

  case object Score extends NestedField with Rawable {
    protected lazy val path: String = TagsFieldType.fieldName
  }
}

// TODO: cetera-etl rename customer_category to domain_category
// A domain category is a domain-specific (customer-specified) category (as
// opposed to a Socrata-specific canonical category).
case object DomainCategoryFieldType extends DocumentFieldType with Countable with Rawable {
  val fieldName: String = "customer_category"
}

case object OwnerIdFieldType extends DocumentFieldType with Countable with NativelyRawable {
  val fieldName: String = "owner_id"
}

case object SharedToFieldType extends DocumentFieldType with NativelyRawable {
  val fieldName: String = "shared_to"
}

case object AttributionFieldType extends DocumentFieldType with Countable with Rawable {
  val fieldName: String = "attribution"
}

/////////////////////
// Catalog Visibility

case object IsCustomerDomainFieldType extends DomainFieldType {
  val fieldName: String = "is_customer_domain"
}

case object IsModerationEnabledFieldType extends DomainFieldType {
  val fieldName: String = "moderation_enabled"
}

case object IsRoutingApprovalEnabledFieldType extends DocumentFieldType {
  val fieldName: String = "routing_approval_enabled"
}

case object IsDefaultViewFieldType extends DocumentFieldType {
  val fieldName: String = "is_default_view"
}

case object IsModerationApprovedFieldType extends DocumentFieldType {
  val fieldName: String = "is_moderation_approved"
}

case object ApprovingDomainIdsFieldType extends DocumentFieldType {
  val fieldName: String = "approving_domain_ids"
}

case object SocrataIdDomainIdFieldType extends DocumentFieldType {
  val fieldName: String = "socrata_id.domain_id"
}

case object ParentDatasetIdFieldType extends DocumentFieldType {
  val fieldName: String = "socrata_id.parent_dataset_id"
}

case object IsPublicFieldType extends DocumentFieldType {
  val fieldName: String = "is_public"
}

case object IsPublishedFieldType extends DocumentFieldType {
  val fieldName: String = "is_published"
}


/////////////////////////////////////////////////
// Domain-specific Categories, Tags, and Metadata

// TODO: cetera-etl rename customer_tags to domain_tags
// Domain tags are customer-defined tags (which surface as topics in the front end).
case object DomainTagsFieldType extends DocumentFieldType with Countable with Rawable {
  val fieldName: String = "customer_tags"
}

// TODO: cetera-etl rename customer_metadata_flattened to domain_metadata_flattened
// domain_metadata_flattened Domain metadata allows customers to define their
// own facets and call them whatever they like, for example you could define
// Superheroes and select from a list of them. We support these fields as
// direct url params (e.g., Superheros=Batman).
case object DomainMetadataFieldType extends DocumentFieldType with Mapable with Rawable {
  val fieldName: String = "customer_metadata_flattened"

  case object Key extends NestedField with Rawable {
    protected lazy val path: String = DomainMetadataFieldType.fieldName
  }

  case object Value extends NestedField with Rawable {
    protected lazy val path: String = DomainMetadataFieldType.fieldName
  }
}


/////////////
// Boostables

case object TitleFieldType extends DocumentFieldType with Boostable with Rawable {
  val fieldName: String = "indexed_metadata.name"
}

case object DescriptionFieldType extends DocumentFieldType with Boostable {
  val fieldName: String = "indexed_metadata.description"
}

case object ColumnNameFieldType extends DocumentFieldType with Boostable {
  val fieldName: String = "indexed_metadata.columns_name"
}

case object ColumnDescriptionFieldType extends DocumentFieldType with Boostable {
  val fieldName: String = "indexed_metadata.columns_description"
}

case object ColumnFieldNameFieldType extends DocumentFieldType with Boostable {
  val fieldName: String = "indexed_metadata.columns_field_name"
}

case object DatatypeFieldType extends DocumentFieldType with Boostable {
  val fieldName: String = "datatype"
}


//////////////
// For sorting

case object PageViewsTotalFieldType extends DocumentFieldType with Sortable {
  val fieldName: String = "page_views.page_views_total"
}

case object PageViewsLastMonthFieldType extends DocumentFieldType with Sortable {
  val fieldName: String = "page_views.page_views_last_month"
}

case object PageViewsLastWeekFieldType extends DocumentFieldType with Sortable {
  val fieldName: String = "page_views.page_views_last_week"
}

case object UpdatedAtFieldType extends DocumentFieldType with Sortable {
  val fieldName: String = "updated_at" // notice the snake_case, long story
}

case object CreatedAtFieldType extends DocumentFieldType with Sortable {
  val fieldName: String = "created_at" // notice the snake_case, long story
}

case object NameFieldType extends DocumentFieldType with Sortable {
  val fieldName: String = "indexed_metadata.name.raw"
}


////////////////
// U'sarians

case object ScreenName extends UserFieldType with Rawable {
  val fieldName: String = "screen_name"
}

case object Email extends UserFieldType with Rawable {
  val fieldName: String = "email"
}
