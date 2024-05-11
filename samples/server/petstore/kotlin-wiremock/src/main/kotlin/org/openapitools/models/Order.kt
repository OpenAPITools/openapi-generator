@file:Suppress(
    "RemoveRedundantQualifierName"
)

package org.openapitools.models

data class Order(
    val id: kotlin.Long,
    val petId: kotlin.Long,
    val quantity: kotlin.Int,
    val shipDate: java.time.OffsetDateTime,
    val status: kotlin.String,
    val complete: kotlin.Boolean,
)
