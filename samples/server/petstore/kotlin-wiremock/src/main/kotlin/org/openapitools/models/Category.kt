@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Category(
    @field:JsonProperty("id")
    val id: kotlin.Long? = null,

    @field:JsonProperty("name")
    val name: kotlin.String? = null,

)
