package org.openapitools.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import org.openapitools._
import java.io.File

/**
 * 
 * @param additionalMetadata Additional data to pass to server
 * @param file file to upload
 */
case class InlineObject1(additionalMetadata: Option[String],
                file: Option[File]
                )

object InlineObject1 {
    /**
     * Creates the codec for converting InlineObject1 from and to JSON.
     */
    implicit val decoder: Decoder[InlineObject1] = deriveDecoder
    implicit val encoder: ObjectEncoder[InlineObject1] = deriveEncoder
}
