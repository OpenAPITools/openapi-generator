package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import javax.validation.Valid
import javax.validation.constraints.*
import io.swagger.annotations.ApiModelProperty

/**
 * An order for a pets from the pet store
 * @param id 
 * @param petId 
 * @param quantity 
 * @param shipDate 
 * @param status Order Status
 * @param complete 
 */
data class Order (

        @ApiModelProperty(value = "")
        @JsonProperty("id") val id: kotlin.Long? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("petId") val petId: kotlin.Long? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("quantity") val quantity: kotlin.Int? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("shipDate") val shipDate: java.time.LocalDateTime? = null,

        @ApiModelProperty(value = "Order Status")
        @JsonProperty("status") val status: Order.Status? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("complete") val complete: kotlin.Boolean? = null
) {

    /**
    * Order Status
    * Values: placed,approved,delivered
    */
    enum class Status(val value: kotlin.String) {
    
        @JsonProperty("placed") placed("placed"),
    
        @JsonProperty("approved") approved("approved"),
    
        @JsonProperty("delivered") delivered("delivered");
    
    }

}

