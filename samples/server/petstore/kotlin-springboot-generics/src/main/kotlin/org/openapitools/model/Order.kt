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
 * 
 * @param orderId 
 * @param quantity 
 * @param totalPrice 
 */
data class Order(

    @get:JsonProperty("orderId", required = true) val orderId: kotlin.String,

    @get:JsonProperty("quantity") val quantity: kotlin.Int? = null,

    @get:JsonProperty("totalPrice") val totalPrice: kotlin.Double? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

