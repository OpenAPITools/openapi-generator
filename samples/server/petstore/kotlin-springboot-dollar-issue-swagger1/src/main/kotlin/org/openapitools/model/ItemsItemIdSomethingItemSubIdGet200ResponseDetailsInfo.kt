package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
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
 * SQ = "; SBS = \; DBS = \\; SD = $some
 * @param detailDollarOne SQ = "; SBS = \; DBS = \\; SD = $some
 * @param detailDollarTwo SQ = "; SBS = \; DBS = \\; SD = $some
 */
data class ItemsItemIdSomethingItemSubIdGet200ResponseDetailsInfo(

    @ApiModelProperty(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("detail\$One") val detailDollarOne: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @ApiModelProperty(example = "42", value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("detail\$Two") val detailDollarTwo: kotlin.Int? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

