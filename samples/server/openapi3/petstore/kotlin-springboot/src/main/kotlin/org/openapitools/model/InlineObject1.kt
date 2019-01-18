package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*
import io.swagger.annotations.ApiModelProperty

/**
 * 
 * @param additionalMetadata Additional data to pass to server
 * @param file file to upload
 */
data class InlineObject1 (

        @ApiModelProperty(example = "null", value = "Additional data to pass to server")
        @JsonProperty("additionalMetadata") val additionalMetadata: String? = null,

        @ApiModelProperty(example = "null", value = "file to upload")
        @JsonProperty("file") val file: java.io.File? = null
) {

}

