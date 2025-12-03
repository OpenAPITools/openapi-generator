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
 * @param dollarId SQ = "; SBS = \; DBS = \\; SD = $some
 * @param dollarName SQ = "; SBS = \; DBS = \\; SD = $some
 */
data class ItemWithDollarAttributesAndExamples(

    @ApiModelProperty(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("\$id") val dollarId: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @ApiModelProperty(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", value = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("\$name") val dollarName: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some"
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

