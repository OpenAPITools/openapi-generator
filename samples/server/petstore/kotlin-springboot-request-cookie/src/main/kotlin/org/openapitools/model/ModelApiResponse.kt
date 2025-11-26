package org.openapitools.model

import java.util.Locale
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
 * 
 * @param code 
 * @param type 
 * @param message 
 */
data class ModelApiResponse(

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("code", required = false)
    val code: kotlin.Int? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("type", required = false)
    val type: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("message", required = false)
    val message: kotlin.String? = null
) {

}

