package io.swagger.client.model

import org.joda.time.DateTime
import java.util.UUID


case class Order (
  id: Option[Long],
petId: Option[Long],
quantity: Option[Integer],
shipDate: Option[DateTime],
status: Option[String],  // Order Status
complete: Option[Boolean]
)
