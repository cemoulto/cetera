package com.socrata.cetera.auth

import org.scalatest.{WordSpec, ShouldMatchers}

class UserSpec extends WordSpec with ShouldMatchers {
  val superAdmin = User("", None, None, Some(List("admin")))
  val customerAdmin = User("", Some("administrator"), None, None)
  val anonymous = User("", None, None, None)

  "The canViewCatalog method" should {
    "return true if the user is an admin" in {
      superAdmin.canViewCatalog should be(true)
    }

    "return true if the user has a role" in {
      customerAdmin.canViewCatalog should be(true)
    }

    "return false if the user isn't an admin and hasn't a role" in {
      anonymous.canViewCatalog should be(false)
    }
  }

  "The canViewUsers method" should {
    "return true if the use is a super admin" in {
      superAdmin.canViewUsers should be(true)
    }

    "return true if the use is a customer admin" in {
      customerAdmin.canViewUsers should be(true)
    }

    "return false if the use is neither type of admin" in {
      anonymous.canViewUsers should be(false)
    }
  }
}
