package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import jakarta.validation.constraints.*
import jakarta.validation.Valid

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
data class Category(

    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @get:Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")
    @get:JsonProperty("name") val name: kotlin.String? = null
) {

}

