package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200ResponseDetailsInfo
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
 * @param itemDollarId SQ = "; SBS = \; DBS = \\; SD = $some
 * @param nameDollarValue SQ = "; SBS = \; DBS = \\; SD = $some
 * @param detailsDollarInfo 
 */
data class ItemsItemIdSomethingItemSubIdGet200Response(

    @ApiModelProperty(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("item\$Id") val itemDollarId: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @ApiModelProperty(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("name\$Value") val nameDollarValue: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @field:Valid
    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("details\$Info") val detailsDollarInfo: ItemsItemIdSomethingItemSubIdGet200ResponseDetailsInfo? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

