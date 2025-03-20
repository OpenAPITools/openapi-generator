@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Pet(
    @field:JsonProperty("name")
    val name: kotlin.String,

    @field:JsonProperty("photoUrls")
    val photoUrls: kotlin.collections.List<kotlin.String>,

    @field:JsonProperty("id")
    val id: kotlin.Long? = null,

    @field:JsonProperty("category")
    val category: Category? = null,

    @field:JsonProperty("tags")
    val tags: kotlin.collections.List<Tag>? = null,

    @field:JsonProperty("status")
    val status: kotlin.String? = null,

)
