@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class NumberPropertiesOnly(
    @field:JsonProperty("number")
    val number: java.math.BigDecimal? = null,

    @field:JsonProperty("float")
    val float: kotlin.Float? = null,

    @field:JsonProperty("double")
    val double: kotlin.Double? = null,

)
