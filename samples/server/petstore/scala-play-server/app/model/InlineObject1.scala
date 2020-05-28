package model

import play.api.libs.json._
import play.api.libs.Files.TemporaryFile

/**
  * Represents the Swagger definition for inline_object_1.
  * @param additionalMetadata Additional data to pass to server
  * @param file file to upload
  */

case class InlineObject1(
  additionalMetadata: Option[String],
  file: Option[TemporaryFile]
)

object InlineObject1 {
  // NOTE: The JSON format for InlineObject1 was not generated because it contains a file variable which cannot be encoded to JSON.
}

