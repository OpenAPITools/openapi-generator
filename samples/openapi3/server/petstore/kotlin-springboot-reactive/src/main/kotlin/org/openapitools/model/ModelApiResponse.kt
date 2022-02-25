package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.annotations.ApiModelProperty

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param type 
 * @param message 
 */
data class ModelApiResponse(

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("code") val code: kotlin.Int? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("type") val type: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @field:JsonProperty("message") val message: kotlin.String? = null
) {

}

