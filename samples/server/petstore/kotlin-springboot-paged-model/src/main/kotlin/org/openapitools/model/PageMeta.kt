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
 * Shared pagination metadata schema
 * @param propertySize 
 * @param number 
 * @param totalElements 
 * @param totalPages 
 */
data class PageMeta(

    @param:JsonProperty("size")
    @get:JsonProperty("size", required = true) val propertySize: kotlin.Long,

    @param:JsonProperty("number")
    @get:JsonProperty("number", required = true) val number: kotlin.Long,

    @param:JsonProperty("totalElements")
    @get:JsonProperty("totalElements", required = true) val totalElements: kotlin.Long,

    @param:JsonProperty("totalPages")
    @get:JsonProperty("totalPages", required = true) val totalPages: kotlin.Long
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

