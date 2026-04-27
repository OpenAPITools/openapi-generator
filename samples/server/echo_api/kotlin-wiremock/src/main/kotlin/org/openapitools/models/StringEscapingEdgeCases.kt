@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class StringEscapingEdgeCases(
    @field:JsonProperty("dollarDefault")
    val dollarDefault: kotlin.String? = "hello \$world, backslash=\\, quote=\", end */",

    @field:JsonProperty("regularProp")
    val regularProp: kotlin.String? = null,

)
