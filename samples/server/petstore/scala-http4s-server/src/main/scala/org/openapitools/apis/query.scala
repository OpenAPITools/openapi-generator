package org.openapitools.apis

import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.syntax.all._

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate

import org.http4s.ParseFailure
import org.http4s.QueryParamDecoder
import org.http4s.QueryParameterValue

import java.time._
import java.util.UUID

object query {
  implicit def rrrefinedQueryParamDecoder[T, P](
    implicit tDecoder: QueryParamDecoder[T], validate: Validate[T, P]
  ): QueryParamDecoder[Refined[T, P]] = new QueryParamDecoder[Refined[T, P]] {
    def decode(value: QueryParameterValue): ValidatedNel[ParseFailure,Refined[T, P]] =
      tDecoder.decode(value).withEither(t => t.flatMap(x =>
        refineV(x).leftMap(err => NonEmptyList.one(ParseFailure(err, err)))
      ))
  }

  abstract class QuerySeqParamDecoderMatcher[T: QueryParamDecoder](name: String) {
    def unapply(params: Map[String, Seq[String]]): Option[List[T]] =
      params
        .get(name)
        .flatMap(values =>
          values.toList.traverse(s => QueryParamDecoder[T].decode(QueryParameterValue(s)).toOption))
  }

  abstract class OptionalQuerySeqParamDecoderMatcher[T: QueryParamDecoder](name: String) {
    def unapply(params: Map[String, List[String]]): Option[Option[List[T]]] =
      params
        .get(name)
        .flatMap(values =>
          values.toList.traverse(s => QueryParamDecoder[T].decode(QueryParameterValue(s)).toOption))
        .fold(List.empty[T].some.some)(_.some.some)
  }

  implicit lazy val BigDecimalQueryParamDecoder: QueryParamDecoder[BigDecimal] =
    QueryParamDecoder.fromUnsafeCast[BigDecimal](x => BigDecimal(x.value))("BigDecimal")

  implicit lazy val LocalDateTimeQueryParamDecoder: QueryParamDecoder[LocalDateTime] =
    QueryParamDecoder.fromUnsafeCast[LocalDateTime](x => LocalDateTime.parse(x.value))("LocalDateTime")

  implicit lazy val LocalDateQueryParamDecoder: QueryParamDecoder[LocalDate] =
    QueryParamDecoder.fromUnsafeCast[LocalDate](x => LocalDate.parse(x.value))("LocalDateTime")

  implicit lazy val ZonedDateTimeQueryParamDecoder: QueryParamDecoder[ZonedDateTime] =
    QueryParamDecoder.fromUnsafeCast[ZonedDateTime](x => ZonedDateTime.parse(x.value))("ZonedDateTime")

  implicit lazy val UUIDQueryParamDecoder: QueryParamDecoder[UUID] =
    QueryParamDecoder.fromUnsafeCast[UUID](x => UUID.fromString(x.value))("UUID")
}