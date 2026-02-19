package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

import org.openapitools.client.models.Annotation

interface FakeApi {
    /**
     * POST fake/annotations
     * annotate
     * 
     * Responses:
     *  - 200: OK
     *
     * @param `annotation` 
     * @return [Call]<[Unit]>
     */
    @POST("fake/annotations")
    fun annotations(@Body `annotation`: Annotation): Call<Unit>

    /**
     * PUT fake/annotations
     * Updates a pet in the store with form data (number)
     * 
     * Responses:
     *  - 405: Invalid input
     *
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet (optional)
     * @param status integer type (optional)
     * @param status2 number type (optional)
     * @return [Call]<[Unit]>
     */
    @FormUrlEncoded
    @PUT("fake/annotations")
    fun updatePetWithFormNumber(@Path("petId") petId: kotlin.Long, @Field("name") name: kotlin.String? = null, @Field("status") status: kotlin.Int? = null, @Field("status2") status2: java.math.BigDecimal? = null): Call<Unit>

}
