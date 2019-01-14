package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*
import io.swagger.annotations.ApiModelProperty

/**
 * Data to update a SysMail template.
 * @param subject 
 * @param body 
 */
data class SysMailFormData (

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("subject") val subject: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("body") val body: String? = null
) {

}

