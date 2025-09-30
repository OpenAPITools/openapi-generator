package org.openapitools.client.infrastructure

import com.squareup.moshi.Moshi
import com.squareup.moshi.adapters.EnumJsonAdapter

object SerializerHelper {
    fun addEnumUnknownDefaultCase(moshiBuilder: Moshi.Builder): Moshi.Builder {
        return moshiBuilder
    }
}
