package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
import org.openapitools.model.LogEntryData
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
 * Log entry wrapper. Same structure as MetricsEntry except 'data' points to LogEntryData. These two form a Tier 3 cluster suggestion. 
 * @param &#x60;data&#x60; 
 * @param severity 
 * @param timestamp 
 */
data class LogEntry(

    @field:Valid
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("data") val `data`: LogEntryData? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("severity") val severity: kotlin.String? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("timestamp") val timestamp: java.time.OffsetDateTime? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

