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
* Values: nameCommaAsc,nameCommaDesc,idCommaAsc,idCommaDesc
*/
enum class PetSortEnum(@get:JsonValue val value: kotlin.String) : java.io.Serializable {

    nameCommaAsc("name,asc"),
    nameCommaDesc("name,desc"),
    idCommaAsc("id,asc"),
    idCommaDesc("id,desc");

    companion object {
        @JvmStatic
        @JsonCreator
        fun forValue(value: kotlin.String): PetSortEnum {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'PetSortEnum'")
        }
    }
}

