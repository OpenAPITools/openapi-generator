package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import rx.Observable
import retrofit2.http.*

import org.openapitools.client.models.Client

interface FakeClassnameTags123Api {
    @PATCH("/fake_classname_test")
    fun testClassname(@Body client: Client): Single<Client>

}
