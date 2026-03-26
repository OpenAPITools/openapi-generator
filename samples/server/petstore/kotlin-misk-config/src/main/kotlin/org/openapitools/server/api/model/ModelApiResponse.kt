package org.openapitools.server.api.model

import com.squareup.moshi.JsonClass

@JsonClass(generateAdapter = true)
data class ModelApiResponse(
    val code: kotlin.Int? = null,
    val type: kotlin.String? = null,
    val message: kotlin.String? = null
)
