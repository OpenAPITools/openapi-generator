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
 * @param propertyB 
 * @param sameNameProperty 
 */
data class Robobird(

    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("discriminator")
    @get:JsonProperty("discriminator", required = true) override val discriminator: kotlin.String = "ROBOBIRD",

    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("another_discriminator")
    @get:JsonProperty("another_discriminator", required = true) override val anotherDiscriminator: kotlin.String = "ANOTHER_ROBOBIRD",

    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("propertyB")
    @get:JsonProperty("propertyB") val propertyB: kotlin.String? = null,

    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("sameNameProperty")
    @get:JsonProperty("sameNameProperty") val sameNameProperty: kotlin.String? = null
) : Animal, AnotherAnimal {

}

