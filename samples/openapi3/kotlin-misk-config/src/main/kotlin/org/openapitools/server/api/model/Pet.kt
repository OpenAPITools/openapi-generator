package org.openapitools.server.api.model

import org.openapitools.server.api.model.Category
import org.openapitools.server.api.model.Tag
import com.squareup.moshi.JsonClass

@JsonClass(generateAdapter = true)
data class Pet(
    val name: kotlin.String,
    val photoUrls: kotlin.collections.List<kotlin.String>,
    val id: kotlin.Long? = null,
    val category: Category? = null,
    val tags: kotlin.collections.List<Tag>? = null,
    /** pet status in the store */
    val status: kotlin.String? = null
)
