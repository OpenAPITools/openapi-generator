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


/**
* A category for a pet
* @param id 
* @param name 
*/


/**
* An order for a pets from the pet store
* @param id 
* @param petId 
* @param quantity 
* @param shipDate 
* @param status Order Status
* @param complete 
*/


/**
* A pet for sale in the pet store
* @param id 
* @param category 
* @param name 
* @param photoUrls 
* @param tags 
* @param status pet status in the store
*/


/**
* A tag for a pet
* @param id 
* @param name 
*/


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


