
package io.swagger.client.model


case class Tag (
    _id: Option[Long],
    _name: Option[String]
)
object Tag {
    def toStringBody(var_id: Object, var_name: Object) =
        s"""
        | {
        | "id":$var_id,"name":$var_name
        | }
        """.stripMargin
}
