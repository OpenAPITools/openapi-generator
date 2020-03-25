package org.openapitools.client.infrastructure

import com.squareup.moshi.JsonDataException
import com.squareup.moshi.Moshi
import retrofit2.Response

inline fun <reified T> Response<*>.getErrorResponse(serializerBuilder: Moshi.Builder = Serializer.moshiBuilder): T? {
    val serializer = serializerBuilder.build()
    val parser = serializer.adapter(T::class.java)
    val response = errorBody()?.string()
    if(response != null)
        try {
            return parser.fromJson(response)
        } catch(e: JsonDataException) {
            e.printStackTrace()
        }
    return null
}
