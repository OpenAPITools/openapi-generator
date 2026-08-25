package com.example.mapped

import com.fasterxml.jackson.annotation.JsonProperty
import java.io.Serializable

/**
 * Handwritten production model used through the Category schema mapping.
 */
data class Category(
    @get:JsonProperty("id")
    val id: Long? = null,
    @get:JsonProperty("name")
    val name: String? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: Long = 1
    }
}
