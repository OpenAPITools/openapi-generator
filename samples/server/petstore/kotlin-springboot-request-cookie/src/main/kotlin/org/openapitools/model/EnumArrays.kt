package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
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
 * @param justSymbol 
 * @param arrayEnum 
 */
data class EnumArrays(

    @Schema(example = "null", description = "")
    @get:JsonProperty("just_symbol") val justSymbol: EnumArrays.JustSymbol? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("array_enum") val arrayEnum: kotlin.collections.List<EnumArrays.ArrayEnum>? = null
) {

    /**
    * 
    * Values: greaterThanEqual,dollar
    */
    enum class JustSymbol(val value: kotlin.String) {

        @JsonProperty(">=") greaterThanEqual(">="),
        @JsonProperty("$") dollar("$")
    }

    /**
    * 
    * Values: fish,crab
    */
    enum class ArrayEnum(val value: kotlin.String) {

        @JsonProperty("fish") fish("fish"),
        @JsonProperty("crab") crab("crab")
    }

}

