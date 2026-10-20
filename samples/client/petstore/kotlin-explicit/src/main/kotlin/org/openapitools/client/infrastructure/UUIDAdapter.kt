package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.util.UUID

public class UUIDAdapter {
    @ToJson
    public fun toJson(uuid: UUID): String = uuid.toString()

    @FromJson
    public fun fromJson(s: String): UUID = UUID.fromString(s)
}
