
package io.swagger.client.model


case class ApiResponse (
    _code: Option[Integer],
    _type: Option[String],
    _message: Option[String]
)
object ApiResponse {
    def toStringBody(var_code: Object, var_type: Object, var_message: Object) =
        s"""
        | {
        | "code":$var_code,"type":$var_type,"message":$var_message
        | }
        """.stripMargin
}
