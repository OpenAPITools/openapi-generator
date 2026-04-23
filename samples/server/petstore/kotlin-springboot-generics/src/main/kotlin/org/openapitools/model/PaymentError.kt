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
 * Payment processing error details
 * @param reason Reason for payment failure
 * @param amount Amount that failed to process
 * @param retryable Whether the payment can be retried
 */
data class PaymentError(

    @get:JsonProperty("reason", required = true) val reason: kotlin.String,

    @get:JsonProperty("amount", required = true) val amount: kotlin.Double,

    @get:JsonProperty("retryable") val retryable: kotlin.Boolean? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

