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
import io.swagger.annotations.ApiModelProperty

/**
 * Describes the result of uploading an image resource
 * @param code 
 * @param type 
 * @param message 
 */
data class ModelApiResponse (

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("code") val code: Int? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("type") val type: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("message") val message: String? = null
) {

}

