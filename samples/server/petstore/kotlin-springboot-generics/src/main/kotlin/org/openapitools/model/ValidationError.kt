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
 * Validation error details
 * @param &#x60;field&#x60; Name of the field that failed validation
 * @param message Human-readable validation error message
 * @param code Machine-readable error code
 */
data class ValidationError(

    @get:JsonProperty("field", required = true) val `field`: kotlin.String,

    @get:JsonProperty("message", required = true) val message: kotlin.String,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("code") val code: kotlin.String? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

