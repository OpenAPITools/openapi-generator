package org.openapitools.server.model

import java.time.OffsetDateTime

/**
 * = Pet Order =
 *
 * An order for a pets from the pet store
 *
 * @param id  for example: ''null''
 * @param petId  for example: ''null''
 * @param quantity  for example: ''null''
 * @param shipDate  for example: ''null''
 * @param status Order Status for example: ''null''
 * @param complete  for example: ''null''
*/
final case class Order (
  id: Option[Long],
  petId: Option[Long],
  quantity: Option[Int],
  shipDate: Option[OffsetDateTime],
  status: Option[String],
  complete: Option[Boolean]
)

