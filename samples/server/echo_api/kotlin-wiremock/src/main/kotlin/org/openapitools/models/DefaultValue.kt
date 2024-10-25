@file:Suppress(
    "RemoveRedundantQualifierName",
    "unused",
)

package org.openapitools.models

import com.fasterxml.jackson.annotation.JsonProperty

data class DefaultValue(
    @field:JsonProperty("array_string_enum_ref_default")
    val arrayStringEnumRefDefault: List<StringEnumRef>? = null,

    @field:JsonProperty("array_string_enum_default")
    val arrayStringEnumDefault: List<String>? = null,

    @field:JsonProperty("array_string_default")
    val arrayStringDefault: List<String>? = arrayListOf("failure","skipped"),

    @field:JsonProperty("array_integer_default")
    val arrayIntegerDefault: List<Int>? = arrayListOf(1,3),

    @field:JsonProperty("array_string")
    val arrayString: List<String>? = null,

    @field:JsonProperty("array_string_nullable")
    val arrayStringNullable: List<String>? = null,

    @field:JsonProperty("array_string_extension_nullable")
    val arrayStringExtensionNullable: List<String>? = null,

    @field:JsonProperty("string_nullable")
    val stringNullable: String? = null,

)
