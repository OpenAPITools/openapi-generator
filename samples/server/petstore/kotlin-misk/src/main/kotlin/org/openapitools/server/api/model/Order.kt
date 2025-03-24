package org.openapitools.server.api.model


data class Order(
    val id: kotlin.Long? = null,
    val petId: kotlin.Long? = null,
    val quantity: kotlin.Int? = null,
    val shipDate: java.time.OffsetDateTime? = null,
    /** Order Status */
    val status: kotlin.String? = null,
    val complete: kotlin.Boolean? = false
)
