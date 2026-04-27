package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
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
 * A pet cat
 * @param huntingSkill The measured skill for hunting
 * @param petType 
 */
data class Cat(

    @get:JsonProperty("huntingSkill", required = true) val huntingSkill: Cat.HuntingSkill,

    @field:Valid
    @get:JsonProperty("petType", required = true) override val petType: kotlin.String = "cat"
) : Pet {

    /**
    * The measured skill for hunting
    * Values: clueless,lazy,adventurous,aggressive
    */
    enum class HuntingSkill(@get:JsonValue val value: kotlin.String) {

        clueless("clueless"),
        lazy("lazy"),
        adventurous("adventurous"),
        aggressive("aggressive");

        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.String): HuntingSkill {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'HuntingSkill'")
            }
        }
    }

}

