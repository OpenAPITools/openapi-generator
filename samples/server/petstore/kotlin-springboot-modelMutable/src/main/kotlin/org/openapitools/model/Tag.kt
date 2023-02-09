package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * A tag for a pet
 * @param id 
 * @param name 
 */
data class Tag(

    @Schema(example = "null", description = "")
    @get:JsonProperty("id") var id: kotlin.Long? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("name") var name: kotlin.String? = null
) {

}

