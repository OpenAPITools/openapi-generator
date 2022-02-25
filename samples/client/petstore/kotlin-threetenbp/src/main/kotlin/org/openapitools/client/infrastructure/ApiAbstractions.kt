package org.openapitools.client.infrastructure

typealias MultiValueMap = MutableMap<String,List<String>>

fun collectionDelimiter(collectionFormat: String) = when(collectionFormat) {
    "csv" -> ","
    "tsv" -> "\t"
    "pipe" -> "|"
    "space" -> " "
    else -> ""
}

val defaultMultiValueConverter: (item: Any?) -> String = { item -> "$item" }

fun <T : Any?> toMultiValue(items: Array<T>, collectionFormat: String, map: (item: T) -> String = defaultMultiValueConverter)
        = toMultiValue(items.asIterable(), collectionFormat, map)

fun <T : Any?> toMultiValue(items: Iterable<T>, collectionFormat: String, map: (item: T) -> String = defaultMultiValueConverter): List<String> {
    return when(collectionFormat) {
        "multi" -> items.map(map)
        else -> listOf(items.joinToString(separator = collectionDelimiter(collectionFormat), transform = map))
    }
}