package com.socrata.cetera.util

trait ParamConverter[T] {
  def convertFrom(s: String): Either[ParamConversionFailure, T]
}

object ParamConverter {
  // Identity implementation for the type class
  implicit object StringParam extends ParamConverter[String] {
    override def convertFrom(s: String): Either[ParamConversionFailure, String] = Right(s)
  }

  private def convertNumber[T](f: (String => T)): ParamConverter[T] =
    new ParamConverter[T] {
      def convertFrom(s: String): Either[ParamConversionFailure, T] =
        try {
          Right(f(s))
        } catch {
          case _: NumberFormatException => Left(InvalidValue(s))
        }
    }

  implicit val intParam = convertNumber(_.toInt)
  implicit val floatParam = convertNumber(_.toFloat)

  def filtered[A: ParamConverter, B](f: A => Option[B]): ParamConverter[B] = {
    new ParamConverter[B] {
      def convertFrom(s: String): Either[ParamConversionFailure, B] = {
        implicitly[ParamConverter[A]].convertFrom(s) match {
          case Right(a) => f(a).map(Right(_)).getOrElse(Left(InvalidValue(s)))
          case Left(e) => Left(e)
        }
      }
    }
  }
}

sealed trait ParamConversionFailure
case class InvalidValue(v: String) extends ParamConversionFailure
