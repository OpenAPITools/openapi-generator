package org.openapitools.client.infrastructure

internal fun collectionDelimiter(collectionFormat: String) = when(collectionFormat) {
    "csv" -> ","
    "tsv" -> "\t"
    "pipe" -> "|"
    "space" -> " "
    else -> ""
}

public sealed class QueryParam {
    public data class Single(val value: String) : QueryParam()
    public data class Multi(val values: Iterable<String>) : QueryParam()
}

@Suppress("MemberVisibilityCanBePrivate", "unused")
public class Queries(public val queries: MutableMap<String, QueryParam> = mutableMapOf()) {
    public constructor(queries: MutableMap<String, QueryParam> = mutableMapOf(), config: Queries.() -> Unit): this(queries) {
        config(this)
    }

    // Single
    public fun add(name: String, value: String?) {
        if (value != null) {
            queries[name] = QueryParam.Single(value)
        }
    }

    public fun add(name: String, value: Any?) {
        if (value != null) {
            queries[name] = QueryParam.Single(value.toString())
        }
    }

    // For Iterables
    public fun addMulti(name: String, values: Iterable<*>, collectionFormat: String) {
        val transformed = values.filterNotNull().map { it.toString() }
        queries[name] = when (collectionFormat) {
            "multi" -> QueryParam.Multi(transformed)
            else -> QueryParam.Single(transformed.joinToString(separator = collectionDelimiter(collectionFormat)))
        }
    }

    // For Arrays
    public fun addMulti(name: String, values: Array<*>, collectionFormat: String): Unit =
        addMulti(name, values.asIterable(), collectionFormat)

    // For Maps
    public fun addMap(values: Map<String, *>): Unit =
        queries.putAll(values.filterValues { it != null }.mapValues { QueryParam.Single(it.value.toString()) })
}
