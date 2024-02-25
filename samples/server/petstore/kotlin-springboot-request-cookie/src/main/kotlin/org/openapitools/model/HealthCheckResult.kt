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
 * Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 * @param nullableMessage 
 */
data class HealthCheckResult(

    @Schema(example = "null", description = "")
    @get:JsonProperty("NullableMessage") val nullableMessage: kotlin.String? = null
) {

}

