package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
data class Category(

    @Schema(example = "null", description = "")
    @get:JsonProperty("id") var id: kotlin.Long? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("name") var name: kotlin.String? = null
) {

}

