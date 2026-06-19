package org.openapitools.apis

import cats.syntax.all._
import cats.data.ValidatedNel

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.util.UUID

import scala.util.Try

object path {
  trait Varrr[T] {
    def unapply(str: String): Option[T]
  }

  implicit val LocalDateVarr: Varrr[LocalDate] = new Varrr[LocalDate] {
    def unapply(str: String): Option[LocalDate] = Try(LocalDate.parse(str)).toOption
  }

  implicit val LocalDateTimeVarr: Varrr[LocalDateTime] = new Varrr[LocalDateTime] {
    def unapply(str: String): Option[LocalDateTime] = Try(LocalDateTime.parse(str)).toOption
  }

  implicit val ZonedDateTimeVarr: Varrr[ZonedDateTime] = new Varrr[ZonedDateTime] {
    def unapply(str: String): Option[ZonedDateTime] = Try(ZonedDateTime.parse(str)).toOption
  }

   implicit val UUIDVarr: Varrr[UUID] = new Varrr[UUID] {
     def unapply(str: String): Option[UUID] = Try(java.util.UUID.fromString(str)).toOption
   }

  implicit val IntVarr: Varrr[Int] = new Varrr[Int] {
    def unapply(str: String): Option[Int] = Try(str.toInt).toOption
  }

  implicit val LongVarr: Varrr[Long] = new Varrr[Long] {
    def unapply(str: String): Option[Long] = Try(str.toLong).toOption
  }

  implicit val DoubleVarr: Varrr[Double] = new Varrr[Double] {
    def unapply(str: String): Option[Double] = Try(str.toDouble).toOption
  }

  implicit val BigDecimalVarr: Varrr[BigDecimal] = new Varrr[BigDecimal] {
    def unapply(str: String): Option[BigDecimal] = Try(BigDecimal(str)).toOption
  }

  implicit val StringVarr: Varrr[String] = new Varrr[String] {
    def unapply(str: String): Option[String] = str.some
  }

  abstract class RefinedVarr[T, P](implicit varrr: Varrr[T], validate: Validate[T, P]) extends Varrr[Refined[T, P]] {
    def unapply(str: String): Option[Refined[T, P]] =
      varrr.unapply(str).flatMap(x => refineV(x).toOption)
  }
}
