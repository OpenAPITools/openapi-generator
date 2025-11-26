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
 * Nested object with $keys.
 * @param detailDollarOne First $detail
 * @param detailDollarTwo Second $detail
 */
data class ItemsItemIdGet200ResponseDetailsInfo(

    @ApiModelProperty(example = "Detail\$1", value = "First \$detail")
    @get:JsonProperty("detail$One") val detailDollarOne: kotlin.String? = null,

    @ApiModelProperty(example = "42", value = "Second \$detail")
    @get:JsonProperty("detail$Two") val detailDollarTwo: kotlin.Int? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

