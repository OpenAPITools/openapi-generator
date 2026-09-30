package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
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

    @param:JsonProperty("id")
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @param:JsonProperty("petId")
    @get:JsonProperty("petId") val petId: kotlin.Long? = null,

    @param:JsonProperty("quantity")
    @get:JsonProperty("quantity") val quantity: kotlin.Int? = null,

    @param:JsonProperty("shipDate")
    @get:JsonProperty("shipDate") val shipDate: java.time.OffsetDateTime? = null,

    @param:JsonProperty("status")
    @get:JsonProperty("status") val status: Order.Status? = null,

    @param:JsonProperty("complete")
    @get:JsonProperty("complete") val complete: kotlin.Boolean? = false
) : java.io.Serializable {

    /**
    * Order Status
    * Values: placed,approved,delivered
    */
    enum class Status(@get:JsonValue val value: kotlin.String) {

        placed("placed"),
        approved("approved"),
        delivered("delivered");

        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.String): Status {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Status'")
            }
        }
    }

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

