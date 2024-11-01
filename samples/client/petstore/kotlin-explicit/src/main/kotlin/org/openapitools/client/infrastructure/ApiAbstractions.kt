package org.openapitools.client.infrastructure

public typealias MultiValueMap = MutableMap<String,List<String>>

public fun collectionDelimiter(collectionFormat: String): String = when(collectionFormat) {
    "csv" -> ","
    "tsv" -> "\t"
    "pipe" -> "|"
    "space" -> " "
    else -> ""
}

public val defaultMultiValueConverter: (item: Any?) -> String = { item -> "$item" }

public fun <T : Any?> toMultiValue(items: Array<T>, collectionFormat: String, map: (item: T) -> String = defaultMultiValueConverter): List<String>
        = toMultiValue(items.asIterable(), collectionFormat, map)

public fun <T : Any?> toMultiValue(items: Iterable<T>, collectionFormat: String, map: (item: T) -> String = defaultMultiValueConverter): List<String> {
    return when(collectionFormat) {
        "multi" -> items.map(map)
        else -> listOf(items.joinToString(separator = collectionDelimiter(collectionFormat), transform = map))
    }
}
