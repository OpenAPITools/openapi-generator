package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*
import io.swagger.annotations.ApiModelProperty

/**
 * 
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 */
data class Body (

        @ApiModelProperty(value = "Updated name of the pet")
        @JsonProperty("name") val name: String? = null,

        @ApiModelProperty(value = "Updated status of the pet")
        @JsonProperty("status") val status: String? = null
) {

}

