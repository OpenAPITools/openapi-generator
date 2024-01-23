package org.openapitools.client.infrastructure

/**
 * Defines a config object for a given part of a multi-part request.
 * NOTE: Headers is a Map<String,String> because rfc2616 defines
 *       multi-valued headers as csv-only.
 */
data class PartConfig<T>(
    val headers: MutableMap<String, String> = mutableMapOf(),
    val body: T? = null
)
