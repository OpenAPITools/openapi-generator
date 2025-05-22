package org.openapitools.server.api.model

import com.squareup.moshi.JsonClass

@JsonClass(generateAdapter = true)
data class Order(
    val id: kotlin.Long? = null,
    val petId: kotlin.Long? = null,
    val quantity: kotlin.Int? = null,
    val shipDate: java.time.OffsetDateTime? = null,
    /** Order Status */
    val status: kotlin.String? = null,
    val complete: kotlin.Boolean? = false
)
