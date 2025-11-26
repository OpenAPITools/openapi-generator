package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import java.io.Serializable
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.annotations.ApiModelProperty

/**
* 
* Values: ENUMVALUE1,ENUMVALUE2,ENUMVALUE3
*/
enum class SomeNullableEnumWithNonNullDefault(@get:JsonValue val value: kotlin.String) {

    ENUMVALUE1("ENUMVALUE1"),
    ENUMVALUE2("ENUMVALUE2"),
    ENUMVALUE3("ENUMVALUE3");

    companion object {
        @JvmStatic
        @JsonCreator
        fun forValue(value: kotlin.String): SomeNullableEnumWithNonNullDefault {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'SomeNullableEnumWithNonNullDefault'")
        }
    }
}

