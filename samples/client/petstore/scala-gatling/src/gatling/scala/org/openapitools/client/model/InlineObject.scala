
package org.openapitools.client.model


case class InlineObject (
    /* Updated name of the pet */
    _name: Option[String],
    /* Updated status of the pet */
    _status: Option[String]
)
object InlineObject {
    def toStringBody(var_name: Object, var_status: Object) =
        s"""
        | {
        | "name":$var_name,"status":$var_status
        | }
        """.stripMargin
}
