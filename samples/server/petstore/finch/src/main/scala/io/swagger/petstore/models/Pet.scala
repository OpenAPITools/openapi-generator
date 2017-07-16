package io.swagger.petstore.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import io.swagger.petstore._
import io.swagger.petstore.models.Category
import io.swagger.petstore.models.Tag
import scala.collection.immutable.Seq

/**
 * A pet for sale in the pet store
 * @param id 
 * @param category 
 * @param name 
 * @param photoUrls 
 * @param tags 
 * @param status pet status in the store
 */
case class Pet(id: Option[Long],
                category: Option[Category],
                name: String,
                photoUrls: Seq[String],
                tags: Option[Seq[Tag]],
                status: Option[String]
                )

object Pet {
    /**
     * Creates the codec for converting Pet from and to JSON.
     */
    implicit val decoder: Decoder[Pet] = deriveDecoder
    implicit val encoder: ObjectEncoder[Pet] = deriveEncoder
}
