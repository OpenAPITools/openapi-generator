@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Bird(
    @field:JsonProperty("size")
    val propertySize: kotlin.String? = null,

    @field:JsonProperty("color")
    val color: kotlin.String? = null,

)
