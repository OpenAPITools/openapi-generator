package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

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

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("id", required = false)
    val id: kotlin.Long? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("petId", required = false)
    val petId: kotlin.Long? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("quantity", required = false)
    val quantity: kotlin.Int? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("shipDate", required = false)
    val shipDate: java.time.OffsetDateTime? = null,

    @Schema(example = "null", required = false, description = "Order Status")
    @get:JsonProperty("status", required = false)
    val status: Order.Status? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("complete", required = false)
    val complete: kotlin.Boolean = false
) {

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
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Order'")
            }
        }
    }

}

