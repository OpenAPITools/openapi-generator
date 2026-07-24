package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.configuration.ValuedEnum
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
 * Values: _10,_20,UNKNOWN_DEFAULT_OPEN_API
 */
enum class ReasonCode(@get:JsonValue override val value: kotlin.Int) : ValuedEnum<kotlin.Int> {
    _10(10),
    _20(20),
    UNKNOWN_DEFAULT_OPEN_API(11184809);

    companion object {
        @JvmStatic
        @JsonCreator
        fun forValue(value: kotlin.Int): ReasonCode {
            return values().firstOrNull{ it.value == value }
                ?: UNKNOWN_DEFAULT_OPEN_API
        }
    }
}
