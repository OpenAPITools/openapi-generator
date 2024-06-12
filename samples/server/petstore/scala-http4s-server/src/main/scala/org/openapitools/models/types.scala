package org.openapitools.models

import java.time._

import io.circe.refined._
import io.circe.syntax._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.string.MatchesRegex
import java.time.ZonedDateTime

/**
* Describes the result of uploading an image resource
* @param code 
* @param _type 
* @param message 
*/

case class ApiResponse(
  code: Option[Int],
  _type: Option[String],
  message: Option[String]
)
object ApiResponse {
  implicit val encoderApiResponse: Encoder[ApiResponse] = deriveEncoder[ApiResponse].mapJson(_.dropNullValues)
  implicit val decoderApiResponse: Decoder[ApiResponse] = deriveDecoder[ApiResponse]
}

/**
* A category for a pet
* @param id 
* @param name 
*/

case class Category(
  id: Option[Long],
  name: Option[Refined[String, MatchesRegex["^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$"]]]
)
object Category {
  implicit val encoderCategory: Encoder[Category] = deriveEncoder[Category].mapJson(_.dropNullValues)
  implicit val decoderCategory: Decoder[Category] = deriveDecoder[Category]
}

/**
* An order for a pets from the pet store
* @param id 
* @param petId 
* @param quantity 
* @param shipDate 
* @param status Order Status
* @param complete 
*/

case class Order(
  id: Option[Long],
  petId: Option[Long],
  quantity: Option[Int],
  shipDate: Option[ZonedDateTime],
  status: Option[String],
  complete: Option[Boolean]
)
object Order {
  implicit val encoderOrder: Encoder[Order] = deriveEncoder[Order].mapJson(_.dropNullValues)
  implicit val decoderOrder: Decoder[Order] = deriveDecoder[Order]
}

/**
* A pet for sale in the pet store
* @param id 
* @param category 
* @param name 
* @param photoUrls 
* @param tags 
* @param status pet status in the store
*/

case class Pet(
  id: Option[Long],
  category: Option[Category],
  name: String,
  photoUrls: List[String],
  tags: Option[List[Tag]],
  status: Option[String]
)
object Pet {
  implicit val encoderPet: Encoder[Pet] = deriveEncoder[Pet].mapJson(_.dropNullValues)
  implicit val decoderPet: Decoder[Pet] = deriveDecoder[Pet]
}

/**
* A tag for a pet
* @param id 
* @param name 
*/

case class Tag(
  id: Option[Long],
  name: Option[String]
)
object Tag {
  implicit val encoderTag: Encoder[Tag] = deriveEncoder[Tag].mapJson(_.dropNullValues)
  implicit val decoderTag: Decoder[Tag] = deriveDecoder[Tag]
}

/**
* A User who is purchasing from the pet store
* @param id 
* @param username 
* @param firstName 
* @param lastName 
* @param email 
* @param password 
* @param phone 
* @param userStatus User Status
*/

case class User(
  id: Option[Long],
  username: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  email: Option[String],
  password: Option[String],
  phone: Option[String],
  userStatus: Option[Int]
)
object User {
  implicit val encoderUser: Encoder[User] = deriveEncoder[User].mapJson(_.dropNullValues)
  implicit val decoderUser: Decoder[User] = deriveDecoder[User]
}

