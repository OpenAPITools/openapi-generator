package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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

    @param:JsonProperty("discriminator")
    @get:JsonProperty("discriminator", required = true) override val discriminator: kotlin.String = "BIRD",

    @param:JsonProperty("another_discriminator")
    @get:JsonProperty("another_discriminator", required = true) override val anotherDiscriminator: kotlin.String = "ANOTHER_BIRD",

    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("propertyA")
    @get:JsonProperty("propertyA") val propertyA: kotlin.String? = null,

    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("sameNameProperty")
    @get:JsonProperty("sameNameProperty") val sameNameProperty: kotlin.Int? = null
) : Animal, AnotherAnimal {

}

