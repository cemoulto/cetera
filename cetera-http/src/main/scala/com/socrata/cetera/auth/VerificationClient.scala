package com.socrata.cetera.auth

import com.socrata.cetera.search.DomainClient
import com.socrata.cetera.types.Domain

class VerificationClient(domainClient: DomainClient, coreClient: CoreClient) {

  // Hi, my name is Werner Brandes. My voice is my passport, verify me.
  def verifyUserAuthorization(
                               cookie: Option[String],
                               domainCname: String,
                               requestId: Option[String],
                               authorized: User => Boolean)
  : (Boolean, Seq[String]) = {
    cookie match {
      case Some(c) =>
        val (authUser, setCookies) = coreClient.fetchUserByCookie(domainCname, c, requestId)
        (authUser.exists(authorized), setCookies)
      case _ =>
        (false, Seq.empty[String])
    }
  }

  /** Gets the search context domains and performs authentication/authorization on the logged-in user.
    *
    * @param domainCname the search context (customer domain)
    * @param cookie the currently logged-in user's core cookie
    * @param requestId a somewhat unique identifier that helps string requests together across services
    * @return (search context domain, elasticsearch timing, whether user is authorized, client cookies to set)
    */
  // TODO: clean this up to separate the two or more things this function performs
  def fetchDomainAndUserAuthorization(
                                       domainCname: Option[String],
                                       cookie: Option[String],
                                       requestId: Option[String],
                                       authorized: User => Boolean)
  : (Option[Domain], Long, Boolean, Seq[String]) =
    domainCname.map { extendedHost =>
      val (domainFound, domainTime) = domainClient.find(extendedHost)
      domainFound.map { domain: Domain =>
        val (authorized, setCookies) = verifyUserAuthorization(cookie, domain.domainCname, requestId, authorized)
        (domainFound, domainTime, authorized, setCookies)
      }.getOrElse((None, domainTime, false, Seq.empty[String]))
    }.getOrElse((None, 0L, false, Seq.empty[String]))

}
