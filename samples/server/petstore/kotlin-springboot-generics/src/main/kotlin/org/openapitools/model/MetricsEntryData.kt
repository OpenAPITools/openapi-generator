package org.openapitools.model

import java.util.Objects
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
 * Payload for a metrics entry
 * @param metricName 
 * @param &#x60;value&#x60; 
 * @param unit 
 */
data class MetricsEntryData(

    @get:JsonProperty("metricName") val metricName: kotlin.String? = null,

    @get:JsonProperty("value") val `value`: kotlin.Double? = null,

    @get:JsonProperty("unit") val unit: kotlin.String? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

