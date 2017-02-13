package io.swagger.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import io.swagger._

/**
 * A tag for a pet
 * @param id 
 * @param name 
 */
case class Tag(id: Option[Long],
                name: Option[String]
                )

object Tag {
    /**
     * Creates the codec for converting Tag from and to JSON.
     */
    implicit val decoder: Decoder[Tag] = deriveDecoder
    implicit val encoder: ObjectEncoder[Tag] = deriveEncoder
}
