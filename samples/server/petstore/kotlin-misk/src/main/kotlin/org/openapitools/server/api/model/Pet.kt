package org.openapitools.server.api.model

import org.openapitools.server.api.model.Category
import org.openapitools.server.api.model.Tag

data class Pet(
    val name: kotlin.String,
    val photoUrls: kotlin.Array<kotlin.String>,
    val id: kotlin.Long?,
    val category: Category?,
    val tags: kotlin.Array<Tag>?,
    /** pet status in the store */
    val status: kotlin.String?
)
