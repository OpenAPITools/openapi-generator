package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import rx.Observable

import org.openapitools.client.models.Client

interface AnotherFakeApi {
    @PATCH("/another-fake/dummy")
    fun call123testSpecialTags(@Body client: Client): Single<Client>

}
