package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import org.openapitools.client.models.InlineResponseDefault

interface DefaultApi {
    @GET("/foo")
    suspend fun fooGet(): InlineResponseDefault

}
