package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid
import io.swagger.annotations.ApiModelProperty

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
data class Category(

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("name") val name: kotlin.String? = null
) {

}

