
package org.openapitools.client.model


case class Category (
    _id: Option[Long],
    _name: Option[String]
)
object Category {
    def toStringBody(var_id: Object, var_name: Object) =
        s"""
        | {
        | "id":$var_id,"name":$var_name
        | }
        """.stripMargin
}
