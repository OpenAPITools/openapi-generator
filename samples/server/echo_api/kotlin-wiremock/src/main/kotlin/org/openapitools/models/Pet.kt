@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Pet(
    @field:JsonProperty("name")
    val name: String,

    @field:JsonProperty("photoUrls")
    val photoUrls: List<String>,

    @field:JsonProperty("id")
    val id: Long? = null,

    @field:JsonProperty("category")
    val category: Category? = null,

    @field:JsonProperty("tags")
    val tags: List<Tag>? = null,

    @field:JsonProperty("status")
    val status: String? = null,

)
