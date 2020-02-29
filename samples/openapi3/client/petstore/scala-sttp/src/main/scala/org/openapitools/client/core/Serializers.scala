package org.openapitools.client.core

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.json4s.{Serializer, CustomSerializer, JNull}
import org.json4s.JsonAST.JString

object Serializers {

  case object DateTimeSerializer extends CustomSerializer[DateTime](_ => ( {
    case JString(s) =>
      ISODateTimeFormat.dateOptionalTimeParser().parseDateTime(s)
    case JNull => null
  }, {
    case d: org.joda.time.DateTime =>
      JString(ISODateTimeFormat.dateTime().print(d))
  })
  )

  case object LocalDateSerializer extends CustomSerializer[org.joda.time.LocalDate](_ => ( {
    case JString(s) => org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd").parseLocalDate(s)
    case JNull => null
  }, {
    case d: org.joda.time.LocalDate => JString(d.toString("yyyy-MM-dd"))
  }))

  def all: Seq[Serializer[_]] = Seq[Serializer[_]]() :+ LocalDateSerializer :+ DateTimeSerializer

}
