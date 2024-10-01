@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Order(
    @field:JsonProperty("id")
    val id: kotlin.Long? = null,

    @field:JsonProperty("petId")
    val petId: kotlin.Long? = null,

    @field:JsonProperty("quantity")
    val quantity: kotlin.Int? = null,

    @field:JsonProperty("shipDate")
    val shipDate: java.time.OffsetDateTime? = null,

    @field:JsonProperty("status")
    val status: kotlin.String? = null,

    @field:JsonProperty("complete")
    val complete: kotlin.Boolean? = false,

)
