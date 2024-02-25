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
 * Model for testing model name starting with number
 * @param name 
 * @param propertyClass 
 */
data class Model200Response(

    @Schema(example = "null", description = "")
    @get:JsonProperty("name") val name: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("class") val propertyClass: kotlin.String? = null
) {

}

