package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
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
    @get:JsonProperty("code") val code: kotlin.Int? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("type") val type: kotlin.String? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("message") val message: kotlin.String? = null
) {

}

