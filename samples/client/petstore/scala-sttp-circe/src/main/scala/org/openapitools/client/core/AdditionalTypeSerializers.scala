package org.openapitools.client.core

import java.net.{ URI, URISyntaxException }

import java.io.File
import java.nio.file.Files
import java.util.Base64

trait AdditionalTypeSerializers {
  import io.circe._

  implicit final lazy val URIDecoder: Decoder[URI] = Decoder.decodeString.emap(string =>
    try Right(new URI(string))
    catch {
      case _: URISyntaxException =>
        Left("String could not be parsed as a URI reference, it violates RFC 2396.")
      case _: NullPointerException =>
        Left("String is null.")
    }
  )

  implicit final lazy val URIEncoder: Encoder[URI] = new Encoder[URI] {
    final def apply(a: URI): Json = Json.fromString(a.toString)
  }

  implicit final lazy val FileDecoder: Decoder[File] = Decoder[Array[Byte]].emap { bytes =>
    try {
      val tmpFile = File.createTempFile("download", ".tmp")
      Files.write(tmpFile.toPath, bytes)
      Right(tmpFile)
    } catch {
      case e: Exception => Left(s"Failed to write binary content to file: ${e.getMessage}")
    }
  }

  implicit final lazy val FileEncoder: Encoder[File] = Encoder[Array[Byte]].contramap(
    f => Files.readAllBytes(f.toPath)
  )

  implicit final lazy val AnyDecoder: Decoder[Any] = Decoder[Json].map(_.asInstanceOf[Any])

  implicit final lazy val AnyEncoder: Encoder[Any] = Encoder.instance {
    case json: Json => json
    case b: Boolean => Json.fromBoolean(b)
    case n: Int => Json.fromInt(n)
    case n: Long => Json.fromLong(n)
    case n: Double => Json.fromDoubleOrNull(n)
    case n: BigDecimal => Json.fromBigDecimal(n)
    case s: String => Json.fromString(s)
    case other => Json.fromString(other.toString)
  }

  implicit final lazy val NanTolerantDoubleDecoder: Decoder[Double] =
    Decoder.decodeDouble.or(Decoder.decodeString.emap {
      case "NaN" => Right(Double.NaN)
      case "Infinity" => Right(Double.PositiveInfinity)
      case "-Infinity" => Right(Double.NegativeInfinity)
      case s => Left(s"Cannot decode '$s' as Double")
  })

  implicit final lazy val Base64OrArrayByteDecoder: Decoder[Array[Byte]] =
    Decoder.decodeArray[Byte].or(Decoder.decodeString.emap { s =>
      try Right(Base64.getDecoder.decode(s))
      catch { case _: IllegalArgumentException => Left(s"Cannot decode '$s' as Base64 Array[Byte]") }
    })
}
