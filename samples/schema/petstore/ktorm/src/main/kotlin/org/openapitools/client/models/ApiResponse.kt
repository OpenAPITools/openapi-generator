/**
* OpenAPI Petstore
* This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
*
* The version of the OpenAPI document: 1.0.0
* 
*
* When I made the templates I intended to import the actual model from kotlin-client 
* to support https://www.ktorm.org/en/define-entities-as-any-kind-of-classes.html
* TODO: implement normal entity https://www.ktorm.org/en/entities-and-column-binding.html
*
*/
package org.openapitools.client.models


import java.io.Serializable

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param type 
 * @param message 
 */

data class ApiResponse (
    //@Json(name = "code")
    val code: kotlin.Int? = null,
    //@Json(name = "type")
    val type: kotlin.String? = null,
    //@Json(name = "message")
    val message: kotlin.String? = null
) : Serializable {
    companion object {
        private const val serialVersionUID: Long = 123
    }

}

