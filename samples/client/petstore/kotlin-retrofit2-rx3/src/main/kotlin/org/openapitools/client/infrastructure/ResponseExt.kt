package org.openapitools.client.infrastructure

import com.squareup.moshi.JsonDataException
import com.squareup.moshi.Moshi
import retrofit2.Response

@Throws(JsonDataException::class)
inline fun <reified T> Response<*>.getErrorResponse(serializerBuilder: Moshi.Builder = Serializer.moshiBuilder): T? {
    val serializer = serializerBuilder.build()
    val parser = serializer.adapter(T::class.java)
    val response = errorBody()?.string()
    if (response != null) {
        return parser.fromJson(response)
    }
    return null
}
