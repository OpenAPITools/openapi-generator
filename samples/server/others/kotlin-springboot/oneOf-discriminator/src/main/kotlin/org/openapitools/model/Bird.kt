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
 * @param discriminator 
 * @param anotherDiscriminator 
 * @param propertyA 
 * @param sameNameProperty 
 */
data class Bird(

    @get:JsonProperty("discriminator", required = true) override val discriminator: kotlin.String = "BIRD",

    @get:JsonProperty("another_discriminator", required = true) override val anotherDiscriminator: kotlin.String = "ANOTHER_BIRD",

    @get:JsonProperty("propertyA") val propertyA: kotlin.String? = null,

    @get:JsonProperty("sameNameProperty") val sameNameProperty: kotlin.Int? = null
) : Animal, AnotherAnimal {

}

