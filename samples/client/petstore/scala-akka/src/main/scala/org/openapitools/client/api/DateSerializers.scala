package org.openapitools.client.core

import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{LocalDate, DateTime}
import org.json4s.{Serializer, CustomSerializer, JNull}
import org.json4s.JsonAST.JString

import scala.util.Try

object DateSerializers {

  case object DateTimeSerializer extends CustomSerializer[DateTime](_ => ( {
    case JString(s) =>
      ISODateTimeFormat.dateOptionalTimeParser().parseDateTime(s)
  }, {
    case d: DateTime => JString(ISODateTimeFormat.dateTime().print(d))
  }))

  case object LocalDateSerializer extends CustomSerializer[LocalDate]( _ => ( {
    case JString(s) => ISODateTimeFormat.localDateParser().parseLocalDate(s)
    }, {
    case d: LocalDate => JString(ISODateTimeFormat.date().print(d))
  }))

 def all: Seq[Serializer[_]] = Seq[Serializer[_]]() :+ DateTimeSerializer :+ LocalDateSerializer

}