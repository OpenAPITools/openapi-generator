package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.Dog
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
 * @param dogType 
 * @param className 
 * @param declawed 
 * @param color 
 * @param breed 
 */
data class BigDog(

    @param:JsonProperty("dogType", required = true)
    @get:JsonProperty("dogType", required = true) val dogType: kotlin.String,

    @param:JsonProperty("className", required = true)
    @get:JsonProperty("className", required = true) override val className: kotlin.String,

    @param:JsonProperty("declawed")
    @get:JsonProperty("declawed") val declawed: kotlin.Boolean? = null,

    @param:JsonProperty("color")
    @get:JsonProperty("color") override val color: kotlin.String? = "red",

    @param:JsonProperty("breed")
    @get:JsonProperty("breed") override val breed: kotlin.String? = null
) : Dog(className = className, breed = breed, color = color) {

}

