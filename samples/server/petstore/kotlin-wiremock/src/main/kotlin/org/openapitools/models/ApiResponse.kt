@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class ApiResponse(
    @field:JsonProperty("code")
    val code: kotlin.Int? = null,

    @field:JsonProperty("type")
    val type: kotlin.String? = null,

    @field:JsonProperty("message")
    val message: kotlin.String? = null,

)
