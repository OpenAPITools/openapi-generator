package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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

    @get:JsonProperty("dogType", required = true) val dogType: kotlin.String,

    @get:JsonProperty("className", required = true) override val className: kotlin.String = "BigDog",

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("declawed") val declawed: kotlin.Boolean? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("color") override val color: kotlin.String? = "red",

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("breed") override val breed: kotlin.String? = null
) : Dog {

}

