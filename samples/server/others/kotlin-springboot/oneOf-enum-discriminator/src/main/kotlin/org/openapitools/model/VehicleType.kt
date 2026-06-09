package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
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
 * Values: CAR,TRUCK,unknown_default_open_api
 */
enum class VehicleType(@get:JsonValue val value: kotlin.String) {
    CAR("CAR"),
    TRUCK("TRUCK"),
    unknown_default_open_api("unknown_default_open_api");

    companion object {
        @JvmStatic
        @JsonCreator
        fun forValue(value: kotlin.String): VehicleType {
            return values().firstOrNull{ it.value == value }
                ?: unknown_default_open_api
        }
    }
}
