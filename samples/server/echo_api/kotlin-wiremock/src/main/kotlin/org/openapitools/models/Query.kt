@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Query(
    @field:JsonProperty("id")
    val id: kotlin.Long? = null,

    @field:JsonProperty("outcomes")
    val outcomes: kotlin.collections.List<kotlin.String>? = null,

)
