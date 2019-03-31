package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*
import io.swagger.annotations.ApiModelProperty

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
data class Category (

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("id") val id: Long? = null,
@get:Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$") 
        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("name") val name: String? = null
) {

}

