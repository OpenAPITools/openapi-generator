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

/**
 * 
 * @param className 
 * @param breed 
 * @param color 
 */
data class Cat(

    @param:JsonProperty("className", required = true)
    @get:JsonProperty("className", required = true) override val className: kotlin.String,

    @param:JsonProperty("breed")
    @get:JsonProperty("breed") val breed: kotlin.String? = null,

    @param:JsonProperty("color")
    @get:JsonProperty("color") override val color: kotlin.String? = "red"
) : Animal {

}

