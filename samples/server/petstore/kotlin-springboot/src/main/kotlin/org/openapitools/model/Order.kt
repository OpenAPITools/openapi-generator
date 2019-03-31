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

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("id") val id: Long? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("petId") val petId: Long? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("quantity") val quantity: Int? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("shipDate") val shipDate: java.time.OffsetDateTime? = null,

        @ApiModelProperty(example = "null", value = "Order Status")
        @JsonProperty("status") val status: Order.Status? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("complete") val complete: Boolean? = null
) {

    /**
    * Order Status
    * Values: placed,approved,delivered
    */
    enum class Status(val value: String) {
    
        @JsonProperty("placed") placed("placed"),
    
        @JsonProperty("approved") approved("approved"),
    
        @JsonProperty("delivered") delivered("delivered");
    
    }

}

