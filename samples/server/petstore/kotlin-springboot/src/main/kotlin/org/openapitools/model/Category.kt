package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.*
import javax.validation.Valid

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
data class Category(

    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @get:JsonProperty("name") val name: kotlin.String? = null
) {

}

