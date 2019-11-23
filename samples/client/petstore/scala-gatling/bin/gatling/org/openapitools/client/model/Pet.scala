
package org.openapitools.client.model


case class Pet (
    _id: Option[Long],
    _category: Option[Category],
    _name: String,
    _photoUrls: List[String],
    _tags: Option[List[Tag]],
    /* pet status in the store */
    _status: Option[String]
)
object Pet {
    def toStringBody(var_id: Object, var_category: Object, var_name: Object, var_photoUrls: Object, var_tags: Object, var_status: Object) =
        s"""
        | {
        | "id":$var_id,"category":$var_category,"name":$var_name,"photoUrls":$var_photoUrls,"tags":$var_tags,"status":$var_status
        | }
        """.stripMargin
}
