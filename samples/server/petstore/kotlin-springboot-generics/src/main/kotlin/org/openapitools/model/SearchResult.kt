package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
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
 * Search result — unique structure, not matched by any pattern
 * @param query 
 * @param totalHits 
 * @param results 
 * @param facets 
 */
data class SearchResult(

    @get:JsonProperty("query") val query: kotlin.String? = null,

    @get:JsonProperty("totalHits") val totalHits: kotlin.Long? = null,

    @get:JsonProperty("results") val results: kotlin.collections.List<kotlin.String>? = null,

    @get:JsonProperty("facets") val facets: kotlin.collections.Map<kotlin.String, kotlin.Int>? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

