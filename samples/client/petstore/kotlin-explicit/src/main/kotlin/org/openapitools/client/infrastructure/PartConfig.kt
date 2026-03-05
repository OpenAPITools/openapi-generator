package org.openapitools.client.infrastructure

/**
 * Defines a config object for a given part of a multi-part request.
 * NOTE: Headers is a Map<String,String> because rfc2616 defines
 *       multi-valued headers as csv-only.
 * 
 * @property headers The headers for this part
 * @property body The body content for this part
 * @property serializer Optional custom serializer for JSON content. When provided, this will be
 *                      used instead of the default serialization for parts with application/json
 *                      content-type. This allows capturing type information at the call site to
 *                      avoid issues with type erasure in kotlinx.serialization.
 */
public data class PartConfig<T>(
    val headers: MutableMap<String, String> = mutableMapOf(),
    val body: T? = null,
    val serializer: ((Any?) -> String)? = null
)
