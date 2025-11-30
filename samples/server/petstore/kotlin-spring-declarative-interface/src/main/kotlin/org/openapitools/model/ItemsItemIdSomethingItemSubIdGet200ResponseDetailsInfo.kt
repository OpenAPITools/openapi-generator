package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
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
 * SQ = "; SBS = \; DBS = \\; SD = $some
 * @param detailDollarOne SQ = "; SBS = \; DBS = \\; SD = $some
 * @param detailDollarTwo SQ = "; SBS = \; DBS = \\; SD = $some
 */
data class ItemsItemIdSomethingItemSubIdGet200ResponseDetailsInfo(

    @Schema(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("detail\$One") val detailDollarOne: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @Schema(example = "42", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("detail\$Two") val detailDollarTwo: kotlin.Int? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

