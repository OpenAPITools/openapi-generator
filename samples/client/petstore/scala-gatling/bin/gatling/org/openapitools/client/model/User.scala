
package org.openapitools.client.model


case class User (
    _id: Option[Long],
    _username: Option[String],
    _firstName: Option[String],
    _lastName: Option[String],
    _email: Option[String],
    _password: Option[String],
    _phone: Option[String],
    /* User Status */
    _userStatus: Option[Integer]
)
object User {
    def toStringBody(var_id: Object, var_username: Object, var_firstName: Object, var_lastName: Object, var_email: Object, var_password: Object, var_phone: Object, var_userStatus: Object) =
        s"""
        | {
        | "id":$var_id,"username":$var_username,"firstName":$var_firstName,"lastName":$var_lastName,"email":$var_email,"password":$var_password,"phone":$var_phone,"userStatus":$var_userStatus
        | }
        """.stripMargin
}
