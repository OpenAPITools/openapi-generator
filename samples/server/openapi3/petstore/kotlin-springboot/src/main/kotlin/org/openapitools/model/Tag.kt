package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.Valid
import javax.validation.constraints.*

/**
 * A tag for a pet
 * @param id 
 * @param name 
 */
data class Tag (
    @JsonProperty("id") val id: kotlin.Long? = null,
    @JsonProperty("name") val name: kotlin.String? = null
) {

}

