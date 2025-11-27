package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.ItemsItemIdGet200ResponseDetailsInfo
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
import io.swagger.annotations.ApiModelProperty

/**
 * 
 * @param itemDollarId The $ID of \"the\" item.
 * @param nameDollarValue The $name \"of the \"item.
 * @param detailsDollarInfo 
 */
data class ItemsItemIdGet200Response(

    @ApiModelProperty(example = "\"item\$123\"", value = "The $ID of \"the\" item.")
    @get:JsonProperty("item$Id") val itemDollarId: kotlin.String? = "Item$Id\"Default\"",

    @ApiModelProperty(example = "\"Item\"\$Name", value = "The $name \"of the \"item.")
    @get:JsonProperty("name$Value") val nameDollarValue: kotlin.String? = "Item$\"NameDefault\"",

    @field:Valid
    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("details$Info") val detailsDollarInfo: ItemsItemIdGet200ResponseDetailsInfo? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

