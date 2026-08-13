package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
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
 * @param fruitType 
 * @param seeds 
 */
data class Apple(

    @param:JsonProperty("fruitType")
    @get:JsonProperty("fruitType", required = true) override val fruitType: kotlin.String = "APPLE",

    @param:JsonProperty("seeds")
    @get:JsonProperty("seeds") val seeds: kotlin.Int? = null
) : Fruit {

}

