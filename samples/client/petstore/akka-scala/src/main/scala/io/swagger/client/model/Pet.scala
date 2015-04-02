package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Pet (
  id: Option[Long],
  category: Option[Category],
  name: String,
  photoUrls: Seq[String],
  tags: Option[Seq[Tag]],
  /* pet status in the store */
  status: Option[PetEnums.Status])
   extends ApiModel

object PetEnums {

  type Status = Status.Value
  
  object Status extends Enumeration {
    val Available = Value("available")
    val Pending = Value("pending")
    val Sold = Value("sold")
  }

  
}

