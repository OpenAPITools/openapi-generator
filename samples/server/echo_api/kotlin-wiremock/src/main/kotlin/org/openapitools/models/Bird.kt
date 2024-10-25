@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class Bird(
    @field:JsonProperty("size")
    val propertySize: String? = null,

    @field:JsonProperty("color")
    val color: String? = null,

)
