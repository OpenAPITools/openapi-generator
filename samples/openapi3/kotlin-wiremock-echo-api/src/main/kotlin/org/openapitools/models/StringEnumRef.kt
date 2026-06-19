@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

enum class StringEnumRef {
    @JsonProperty(value = "success") success,
    @JsonProperty(value = "failure") failure,
    @JsonProperty(value = "unclassified") unclassified,
}
