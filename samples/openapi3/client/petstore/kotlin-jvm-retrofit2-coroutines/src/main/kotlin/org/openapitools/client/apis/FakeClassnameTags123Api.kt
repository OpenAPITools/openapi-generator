package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import org.openapitools.client.models.Client

interface FakeClassnameTags123Api {
    @PATCH("/fake_classname_test")
    suspend fun testClassname(@Body client: Client): Client

}
