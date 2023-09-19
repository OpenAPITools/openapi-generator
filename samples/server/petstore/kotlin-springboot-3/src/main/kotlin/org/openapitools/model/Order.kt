package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
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
 * An order for a pets from the pet store
 * @param id 
 * @param petId 
 * @param quantity 
 * @param shipDate 
 * @param status Order Status
 * @param complete 
 */
data class Order(

    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @get:JsonProperty("petId") val petId: kotlin.Long? = null,

    @get:JsonProperty("quantity") val quantity: kotlin.Int? = null,

    @get:JsonProperty("shipDate") val shipDate: java.time.OffsetDateTime? = null,

    @get:JsonProperty("status") val status: Order.Status? = null,

    @get:JsonProperty("complete") val complete: kotlin.Boolean? = false
) {

    /**
    * Order Status
    * Values: placed,approved,delivered
    */
    enum class Status(val value: kotlin.String) {

        @JsonProperty("placed") placed("placed"),
        @JsonProperty("approved") approved("approved"),
        @JsonProperty("delivered") delivered("delivered")
    }

}

