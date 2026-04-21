package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.PageMeta
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid

/**
 * Search result with metadata — no 'content' array at all
 * @param query 
 * @param totalHits 
 * @param page 
 */
data class SearchResult(

    @get:JsonProperty("query") val query: kotlin.String? = null,

    @get:JsonProperty("totalHits") val totalHits: kotlin.Int? = null,

    @field:Valid
    @get:JsonProperty("page") val page: PageMeta? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

