package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

import org.openapitools.client.models.Animal

interface BirdApi {
    /**
     * GET v1/bird/{id}
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @param id 
     * @return [Call]<[Animal]>
     */
    @GET("v1/bird/{id}")
    fun getBird(@Path("id") id: java.util.UUID): Call<Animal>

}
