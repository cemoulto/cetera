package com.socrata.cetera.auth

import com.rojoma.json.v3.util.AutomaticJsonCodecBuilder

case class User(id: String,
                roleName: Option[String],
                rights: Option[Seq[String]],
                flags: Option[Seq[String]]) {

  def isAdmin: Boolean = flags.exists(_.contains("admin"))
  def hasRole: Boolean = roleName.nonEmpty
  def hasRole(role: String): Boolean = roleName.contains(role)

  def canViewCatalog: Boolean = hasRole || isAdmin
  def canViewUsers: Boolean = hasRole("administrator") || isAdmin
}

object User {
  implicit val jCodec = AutomaticJsonCodecBuilder[User]
}
