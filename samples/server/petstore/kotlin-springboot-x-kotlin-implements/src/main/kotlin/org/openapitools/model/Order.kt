package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import java.io.Serializable
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid

/**
 * 
 * @param id 
 * @param petId 
 * @param quantity 
 * @param shipDate 
 * @param status 
 * @param complete 
 */
data class Order(

    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @get:JsonProperty("petId") val petId: kotlin.Long? = null,

    @get:JsonProperty("quantity") val quantity: kotlin.Int? = null,

    @get:JsonProperty("shipDate") val shipDate: java.time.OffsetDateTime? = null,

    @get:JsonProperty("status") val status: Order.Status? = null,

    @get:JsonProperty("complete") val complete: kotlin.Boolean? = false
) : Serializable {

    /**
    * 
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
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Order'")
            }
        }
    }

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

