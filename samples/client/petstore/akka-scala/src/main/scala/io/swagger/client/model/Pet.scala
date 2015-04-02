package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Pet (
  Id: Option[Long],
  Category: Option[Category],
  Name: String,
  PhotoUrls: Seq[String],
  Tags: Option[Seq[Tag]],
  /* pet status in the store */
  Status: Option[PetEnums.Status])
   extends ApiModel

object PetEnums {

  type Status = Status.Value
  
  object Status extends Enumeration {
    val Available = Value("available")
    val Pending = Value("pending")
    val Sold = Value("sold")
  }

  
}

