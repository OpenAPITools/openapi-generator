package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200ResponseDetailsInfo
import java.io.Serializable
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * 
 * @param itemDollarId SQ = "; SBS = \; DBS = \\; SD = $some
 * @param nameDollarValue SQ = "; SBS = \; DBS = \\; SD = $some
 * @param detailsDollarInfo 
 */
data class ItemsItemIdSomethingItemSubIdGet200Response(

    @Schema(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("item\$Id") val itemDollarId: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @Schema(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("name\$Value") val nameDollarValue: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("details\$Info") val detailsDollarInfo: ItemsItemIdSomethingItemSubIdGet200ResponseDetailsInfo? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

