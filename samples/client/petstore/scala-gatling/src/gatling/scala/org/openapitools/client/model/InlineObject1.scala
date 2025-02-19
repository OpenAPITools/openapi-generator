
package org.openapitools.client.model

import java.io.File

case class InlineObject1 (
    /* Additional data to pass to server */
    _additionalMetadata: Option[String],
    /* file to upload */
    _file: Option[File]
)
object InlineObject1 {
    def toStringBody(var_additionalMetadata: Object, var_file: Object) =
        s"""
        | {
        | "additionalMetadata":$var_additionalMetadata,"file":$var_file
        | }
        """.stripMargin
}
