@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Query(
    @field:JsonProperty("id")
    val id: Long? = null,

    @field:JsonProperty("outcomes")
    val outcomes: List<String>? = null,

)
