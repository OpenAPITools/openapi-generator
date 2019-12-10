package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call

import org.openapitools.client.models.ApiResponse
import org.openapitools.client.models.Pet

interface PetApi {
    @POST("/pet")
    fun addPet(@Body body: Pet): Call<Unit>

    @DELETE("/pet/{petId}")
    fun deletePet(@Path("petId") petId: kotlin.Long, @Header("api_key") apiKey: kotlin.String): Call<Unit>

    @GET("/pet/findByStatus")
    fun findPetsByStatus(@Query("status") status: CSVParams): Call<kotlin.Array<Pet>>

    @Deprecated
    @GET("/pet/findByTags")
    fun findPetsByTags(@Query("tags") tags: CSVParams): Call<kotlin.Array<Pet>>

    @GET("/pet/{petId}")
    fun getPetById(@Path("petId") petId: kotlin.Long): Call<Pet>

    @PUT("/pet")
    fun updatePet(@Body body: Pet): Call<Unit>

    @FormUrlEncoded
    @POST("/pet/{petId}")
    fun updatePetWithForm(@Path("petId") petId: kotlin.Long, @Field("name") name: kotlin.String, @Field("status") status: kotlin.String): Call<Unit>

    @Multipart
    @POST("/pet/{petId}/uploadImage")
    fun uploadFile(@Path("petId") petId: kotlin.Long, @Part("additionalMetadata") additionalMetadata: kotlin.String, @Part file: MultipartBody.Part ): Call<ApiResponse>

}
