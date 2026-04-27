package org.openapitools.server.api.model

import com.squareup.moshi.JsonClass

@JsonClass(generateAdapter = true)
data class EscapingEdgeCases(
    /** A string whose default has $dollar, backslash \\, quote \&quot; and comment-close *_/ */
    val dollarDefault: kotlin.String? = "hello \$world, backslash=\\, quote=\", end */",
    /** Regular property for baseline comparison */
    val regularProp: kotlin.String? = null
)
