package org.openapitools.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import org.openapitools._

/**
 * 
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 */
case class InlineObject(name: Option[String],
                status: Option[String]
                )

object InlineObject {
    /**
     * Creates the codec for converting InlineObject from and to JSON.
     */
    implicit val decoder: Decoder[InlineObject] = deriveDecoder
    implicit val encoder: ObjectEncoder[InlineObject] = deriveEncoder
}
