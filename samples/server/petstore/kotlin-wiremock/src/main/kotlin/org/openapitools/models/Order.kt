@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Order(
    @field:JsonProperty("id")
    val id: Long? = null,

    @field:JsonProperty("petId")
    val petId: Long? = null,

    @field:JsonProperty("quantity")
    val quantity: Int? = null,

    @field:JsonProperty("shipDate")
    val shipDate: OffsetDateTime? = null,

    @field:JsonProperty("status")
    val status: String? = null,

    @field:JsonProperty("complete")
    val complete: Boolean? = false,

)
