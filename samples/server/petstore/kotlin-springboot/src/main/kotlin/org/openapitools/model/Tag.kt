package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*
import io.swagger.annotations.ApiModelProperty

/**
 * A tag for a pet
 * @param id 
 * @param name 
 */
data class Tag (

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("id") val id: Long? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("name") val name: String? = null
) {

}

