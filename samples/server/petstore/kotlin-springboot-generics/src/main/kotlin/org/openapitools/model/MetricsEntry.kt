package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
import org.openapitools.model.MetricsEntryData
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
 * Metrics entry wrapper. Same structure as LogEntry except 'data' points to MetricsEntryData. These two form a Tier 3 cluster suggestion. 
 * @param &#x60;data&#x60; 
 * @param severity 
 * @param timestamp 
 */
data class MetricsEntry(

    @field:Valid
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("data") val `data`: MetricsEntryData? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("severity") val severity: kotlin.String? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("timestamp") val timestamp: java.time.OffsetDateTime? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

