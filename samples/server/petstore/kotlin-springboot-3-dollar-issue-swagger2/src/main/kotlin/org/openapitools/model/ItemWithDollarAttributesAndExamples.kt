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
 * @param dollarId SQ = "; SBS = \; DBS = \\; SD = $some
 * @param dollarName SQ = "; SBS = \; DBS = \\; SD = $some
 */
data class ItemWithDollarAttributesAndExamples(

    @Schema(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("\$id") val dollarId: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some",

    @Schema(example = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some", description = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
    @get:JsonProperty("\$name") val dollarName: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some"
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

