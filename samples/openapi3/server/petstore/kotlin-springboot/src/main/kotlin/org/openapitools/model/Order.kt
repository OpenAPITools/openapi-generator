package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
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
data class Order(

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("id") val id: kotlin.Long? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("petId") val petId: kotlin.Long? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("quantity") val quantity: kotlin.Int? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("shipDate") val shipDate: java.time.OffsetDateTime? = null,

    @ApiModelProperty(example = "null", value = "Order Status")
    @field:JsonProperty("status") val status: Order.Status? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("complete") val complete: kotlin.Boolean? = false
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

