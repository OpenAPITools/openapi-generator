package io.swagger.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import io.swagger._
import java.time.ZonedDateTime

/**
 * An order for a pets from the pet store
 * @param id 
 * @param petId 
 * @param quantity 
 * @param shipDate 
 * @param status Order Status
 * @param complete 
 */
case class Order(id: Option[Long],
                petId: Option[Long],
                quantity: Option[Int],
                shipDate: Option[ZonedDateTime],
                status: Option[String],
                complete: Option[Boolean]
                )

object Order {
    /**
     * Creates the codec for converting Order from and to JSON.
     */
    implicit val decoder: Decoder[Order] = deriveDecoder
    implicit val encoder: ObjectEncoder[Order] = deriveEncoder
}
