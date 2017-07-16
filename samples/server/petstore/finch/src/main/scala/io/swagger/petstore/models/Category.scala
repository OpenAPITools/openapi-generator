package io.swagger.petstore.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import io.swagger.petstore._

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
case class Category(id: Option[Long],
                name: Option[String]
                )

object Category {
    /**
     * Creates the codec for converting Category from and to JSON.
     */
    implicit val decoder: Decoder[Category] = deriveDecoder
    implicit val encoder: ObjectEncoder[Category] = deriveEncoder
}
