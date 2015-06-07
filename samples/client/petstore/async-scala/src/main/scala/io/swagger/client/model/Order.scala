package io.swagger.client.model


case class Order(
                  id: Long,
                  petId: Long,
                  quantity: Integer,
                  shipDate: DateTime,
                  status: String, // Order Status
                  complete: Boolean

                  )
