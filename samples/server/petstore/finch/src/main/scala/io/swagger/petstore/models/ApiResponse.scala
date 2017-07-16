package io.swagger.petstore.models

import io.circe._
import io.finch.circe._
import io.circe.generic.semiauto._
import io.circe.java8.time._
import io.swagger.petstore._

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param _type 
 * @param message 
 */
case class ApiResponse(code: Option[Int],
                _type: Option[String],
                message: Option[String]
                )

object ApiResponse {
    /**
     * Creates the codec for converting ApiResponse from and to JSON.
     */
    implicit val decoder: Decoder[ApiResponse] = deriveDecoder
    implicit val encoder: ObjectEncoder[ApiResponse] = deriveEncoder
}
