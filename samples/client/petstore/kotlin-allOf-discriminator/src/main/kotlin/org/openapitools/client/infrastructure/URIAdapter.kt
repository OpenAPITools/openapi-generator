package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.net.URI

class URIAdapter {
    @ToJson
    fun toJson(uri: URI): String = uri.toString()

    @FromJson
    fun fromJson(s: String): URI = URI.create(s)
}
