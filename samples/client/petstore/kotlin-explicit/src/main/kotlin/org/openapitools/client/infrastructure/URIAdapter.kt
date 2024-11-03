package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.net.URI

public class URIAdapter {
    @ToJson
    public fun toJson(uri: URI): String = uri.toString()

    @FromJson
    public fun fromJson(s: String): URI = URI.create(s)
}
