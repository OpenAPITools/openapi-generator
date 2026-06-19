@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class DefaultValue(
    @field:JsonProperty("array_string_enum_ref_default")
    val arrayStringEnumRefDefault: kotlin.collections.List<StringEnumRef>? = null,

    @field:JsonProperty("array_string_enum_default")
    val arrayStringEnumDefault: kotlin.collections.List<kotlin.String>? = null,

    @field:JsonProperty("array_string_default")
    val arrayStringDefault: kotlin.collections.List<kotlin.String>? = arrayListOf("failure","skipped"),

    @field:JsonProperty("array_integer_default")
    val arrayIntegerDefault: kotlin.collections.List<kotlin.Int>? = arrayListOf(1,3),

    @field:JsonProperty("array_string")
    val arrayString: kotlin.collections.List<kotlin.String>? = null,

    @field:JsonProperty("array_string_nullable")
    val arrayStringNullable: kotlin.collections.List<kotlin.String>? = null,

    @field:JsonProperty("array_string_extension_nullable")
    val arrayStringExtensionNullable: kotlin.collections.List<kotlin.String>? = null,

    @field:JsonProperty("string_nullable")
    val stringNullable: kotlin.String? = null,

)
