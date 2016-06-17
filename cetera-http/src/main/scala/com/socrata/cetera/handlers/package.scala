package com.socrata.cetera

import com.socrata.http.server.HttpRequest

package object util {
  type MultiQueryParams = Map[String, Seq[String]]

  implicit class HttpRequestCompanion(req: HttpRequest) {
    def multiQueryParams: MultiQueryParams =
      req.allQueryParameters.mapValues(_.flatten)
  }

  implicit class TypedQueryParams(queryParameters: MultiQueryParams) {
    def first(name: String): Option[String] = {
      queryParameters.get(name).flatMap(_.headOption)
    }

    def typedSeq[T: ParamConverter](name: String): Option[Seq[Either[ParamConversionFailure, T]]] = {
      queryParameters.get(name).map {
        _.map(implicitly[ParamConverter[T]].convertFrom)
      }
    }

    def typedFirst[T: ParamConverter](name: String): Option[Either[ParamConversionFailure, T]] = {
      queryParameters.typedSeq[T](name).flatMap(_.headOption)
    }

    def typedFirstOrElse[T: ParamConverter](name: String, default: T): Either[ParamConversionFailure, T] = {
      typedFirst[T](name).getOrElse(Right(default))
    }
  }
}
