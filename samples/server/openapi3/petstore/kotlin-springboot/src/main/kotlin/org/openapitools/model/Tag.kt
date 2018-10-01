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

        @ApiModelProperty(value = "")
        @JsonProperty("id") val id: Long? = null,

        @ApiModelProperty(value = "")
        @JsonProperty("name") val name: String? = null
) {

}

