package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param type 
 * @param message 
 */
data class ModelApiResponse (
    @JsonProperty("code") val code: kotlin.Int? = null,
    @JsonProperty("type") val type: kotlin.String? = null,
    @JsonProperty("message") val message: kotlin.String? = null
) {

}

