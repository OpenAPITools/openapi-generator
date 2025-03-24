package org.openapitools.server.api.model

import org.openapitools.server.api.model.Category
import org.openapitools.server.api.model.Tag

data class Pet(
    val name: kotlin.String,
    val photoUrls: kotlin.Array<kotlin.String>,
    val id: kotlin.Long? = null,
    val category: Category? = null,
    val tags: kotlin.Array<Tag>? = null,
    /** pet status in the store */
    val status: kotlin.String? = null
)
