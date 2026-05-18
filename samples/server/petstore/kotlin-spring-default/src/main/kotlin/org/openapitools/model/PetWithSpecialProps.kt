package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * Model with $-prefixed property names for escaping tests (Issue 20502)
 * @param dollarId Property with $-prefix in name; default has $dollar and backslash \\
 * @param nameDollarSuffix Property with $ mid-name; description has backslash \\ and quote \"
 */
data class PetWithSpecialProps(

    @Schema(example = "null", description = "Property with \$-prefix in name; default has \$dollar and backslash \\")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("\$id") val dollarId: kotlin.String? = "hello \$world",

    @Schema(example = "null", description = "Property with \$ mid-name; description has backslash \\ and quote \"")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("name\$Suffix") val nameDollarSuffix: kotlin.String? = null
) {

}

