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
* Values: idCommaAsc,idCommaDesc,createdAtCommaAsc,createdAtCommaDesc
*/
enum class PetSort(@get:JsonValue val value: kotlin.String) : java.io.Serializable {

    idCommaAsc("id,asc"),
    idCommaDesc("id,desc"),
    createdAtCommaAsc("createdAt,asc"),
    createdAtCommaDesc("createdAt,desc");

    companion object {
        @JvmStatic
        @JsonCreator
        fun forValue(value: kotlin.String): PetSort {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'PetSort'")
        }
    }
}

