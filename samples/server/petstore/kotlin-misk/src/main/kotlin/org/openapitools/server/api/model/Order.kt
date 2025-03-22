package org.openapitools.server.api.model


data class Order(
    val id: kotlin.Long?,
    val petId: kotlin.Long?,
    val quantity: kotlin.Int?,
    val shipDate: java.time.OffsetDateTime?,
    /** Order Status */
    val status: kotlin.String?,
    val complete: kotlin.Boolean? = false
)
