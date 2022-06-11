
package org.openapitools.client.model

import java.util.Date

case class Order (
    _id: Option[Long],
    _petId: Option[Long],
    _quantity: Option[Integer],
    _shipDate: Option[Date],
    /* Order Status */
    _status: Option[String],
    _complete: Option[Boolean]
)
object Order {
    def toStringBody(var_id: Object, var_petId: Object, var_quantity: Object, var_shipDate: Object, var_status: Object, var_complete: Object) =
        s"""
        | {
        | "id":$var_id,"petId":$var_petId,"quantity":$var_quantity,"shipDate":$var_shipDate,"status":$var_status,"complete":$var_complete
        | }
        """.stripMargin
}
