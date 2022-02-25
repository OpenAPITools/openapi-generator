package org.openapitools.server.model

import java.io.File

/**
 * @param additionalMetadata Additional data to pass to server for example: ''null''
 * @param file file to upload for example: ''null''
*/
final case class InlineObject1 (
  additionalMetadata: Option[String],
  file: Option[File]
)

