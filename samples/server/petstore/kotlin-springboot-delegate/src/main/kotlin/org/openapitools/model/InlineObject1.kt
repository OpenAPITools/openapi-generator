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
 * 
 * @param additionalMetadata Additional data to pass to server
 * @param file file to upload
 */
data class InlineObject1(

    @ApiModelProperty(example = "null", value = "Additional data to pass to server")
    @field:JsonProperty("additionalMetadata") val additionalMetadata: kotlin.String? = null,

    @field:Valid
    @ApiModelProperty(example = "null", value = "file to upload")
    @field:JsonProperty("file") val file: org.springframework.core.io.Resource? = null
) {

}

