@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

data class Pet(
    val name: kotlin.String,
    val photoUrls: kotlin.collections.List<kotlin.String>,
    val id: kotlin.Long?,
    val category: Category?,
    val tags: kotlin.collections.List<Tag>?,
    val status: kotlin.String?,
)
