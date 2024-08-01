package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param type 
 * @param message 
 */
data class ModelApiResponse(

    @Schema(example = "null", description = "")
    @get:JsonProperty("code") val code: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("type") val type: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("message") val message: kotlin.String? = null
    ) {

}

