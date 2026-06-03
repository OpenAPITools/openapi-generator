package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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
 * Payload for a log entry
 * @param level 
 * @param message 
 * @param source 
 */
data class LogEntryData(

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("level") val level: kotlin.String? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("message") val message: kotlin.String? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("source") val source: kotlin.String? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

