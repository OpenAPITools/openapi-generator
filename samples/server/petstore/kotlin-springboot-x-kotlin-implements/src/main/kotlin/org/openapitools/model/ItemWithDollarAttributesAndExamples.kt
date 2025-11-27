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
 * A schema \"demonstrating\" $usage in properties.
 * @param dollarId \"$ID property\"
 * @param dollarName $Name\" property\"
 */
data class ItemWithDollarAttributesAndExamples(

    @ApiModelProperty(example = "\$item\"123\"", value = "\"$ID property\"")
    @get:JsonProperty("$id") val dollarId: kotlin.String? = "$item\"123Default\"",

    @ApiModelProperty(example = "\$Item\"Name", value = "$Name\" property\"")
    @get:JsonProperty("$name") val dollarName: kotlin.String? = "$ItemNameDefault\""
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

