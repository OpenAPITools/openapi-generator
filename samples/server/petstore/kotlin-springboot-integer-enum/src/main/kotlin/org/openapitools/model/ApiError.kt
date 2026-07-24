package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.annotation.Nulls
import org.openapitools.model.ReasonCode
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
 * @param errorCode 
 * @param reasonCode 
 */
data class ApiError(

    @param:JsonProperty("errorCode")
    @get:JsonProperty("errorCode", required = true) val errorCode: ApiError.ErrorCode,

    @field:Valid
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("reasonCode")
    @get:JsonProperty("reasonCode") val reasonCode: ReasonCode? = null
) {

    /**
    * 
    * Values: OK,ERROR
    */
    enum class ErrorCode(@get:JsonValue override val value: kotlin.Int) : ValuedEnum<kotlin.Int> {

        OK(0),
        ERROR(100);

        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.Int): ErrorCode {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'ErrorCode'")
            }
        }
    }

}

