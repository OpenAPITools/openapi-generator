package org.openapitools.client.core

import java.net.{ URI, URISyntaxException }

object AdditionalTypeSerializers {
    import org.json4s.{Serializer, CustomSerializer, JNull, MappingException}
    import org.json4s.JsonAST.JString
      case object URISerializer extends CustomSerializer[URI]( _ => ( {
        case JString(s) => 
          try new URI(s)
          catch {
            case _: URISyntaxException =>
              throw new MappingException("String could not be parsed as a URI reference, it violates RFC 2396.")
            case _: NullPointerException =>
               throw new MappingException("String is null.")
          }
        case JNull => null
      }, {
        case uri: URI =>
         JString(uri.toString())
      }))

  def all: Seq[Serializer[_]] = Seq[Serializer[_]]() :+ URISerializer
}
