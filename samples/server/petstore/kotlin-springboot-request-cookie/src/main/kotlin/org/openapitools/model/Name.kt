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
 * Model for testing model name same as property name
 * @param name 
 * @param snakeCase 
 * @param &#x60;property&#x60; 
 * @param &#x60;123number&#x60; 
 */
data class Name(

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("name", required = true) val name: kotlin.Int,

    @Schema(example = "null", readOnly = true, description = "")
    @get:JsonProperty("snake_case") val snakeCase: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("property") val `property`: kotlin.String? = null,

    @Schema(example = "null", readOnly = true, description = "")
    @get:JsonProperty("123Number") val `123number`: kotlin.Int? = null
) {

}

