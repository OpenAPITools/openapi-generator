package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.ApiAnnotation

interface FakeApi {
    /**
     * annotate
     * 
     * Responses:
     *  - 200: OK
     *
     * @param apiAnnotation 
     * @return [Unit]
     */
    @POST("fake/annotations")
    suspend fun annotations(@Body apiAnnotation: ApiAnnotation): Response<Unit>

}
