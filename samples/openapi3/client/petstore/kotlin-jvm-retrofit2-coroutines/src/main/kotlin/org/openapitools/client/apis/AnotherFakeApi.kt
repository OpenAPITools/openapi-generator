package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import retrofit2.http.*

import org.openapitools.client.models.Client

interface AnotherFakeApi {
    @PATCH("/another-fake/dummy")
    suspend fun call123testSpecialTags(@Body client: Client): Client

}
