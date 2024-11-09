@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class NumberPropertiesOnly(
    @field:JsonProperty("number")
    val number: BigDecimal? = null,

    @field:JsonProperty("float")
    val float: Float? = null,

    @field:JsonProperty("double")
    val double: Double? = null,

)
