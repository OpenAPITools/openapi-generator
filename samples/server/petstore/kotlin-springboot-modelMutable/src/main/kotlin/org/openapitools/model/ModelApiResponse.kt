package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param type 
 * @param message 
 */
data class ModelApiResponse(

    @Schema(example = "null", description = "")
    @get:JsonProperty("code") var code: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("type") var type: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("message") var message: kotlin.String? = null
) {

}

