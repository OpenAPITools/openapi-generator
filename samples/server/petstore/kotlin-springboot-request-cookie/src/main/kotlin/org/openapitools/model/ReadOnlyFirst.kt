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
 * 
 * @param bar 
 * @param baz 
 */
data class ReadOnlyFirst(

    @Schema(example = "null", readOnly = true, description = "")
    @get:JsonProperty("bar") val bar: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("baz") val baz: kotlin.String? = null
) {

}

