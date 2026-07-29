package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.Animal
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
 * @param className 
 * @param declawed 
 * @param color 
 */
data class Cat(

    @Schema(required = true, description = "")
    @param:JsonProperty("className")
    @get:JsonProperty("className", required = true) override val className: kotlin.String,

    @Schema(description = "")
    @param:JsonProperty("declawed")
    @get:JsonProperty("declawed") val declawed: kotlin.Boolean? = null,

    @Schema(description = "")
    @param:JsonProperty("color")
    @get:JsonProperty("color") override val color: kotlin.String? = "red"
) : Animal {

}

