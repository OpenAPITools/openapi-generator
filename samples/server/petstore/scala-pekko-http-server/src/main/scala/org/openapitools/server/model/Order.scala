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
  id: Option[Long] = None,
  petId: Option[Long] = None,
  quantity: Option[Int] = None,
  shipDate: Option[OffsetDateTime] = None,
  status: Option[String] = None,
  complete: Option[Boolean] = None
)

