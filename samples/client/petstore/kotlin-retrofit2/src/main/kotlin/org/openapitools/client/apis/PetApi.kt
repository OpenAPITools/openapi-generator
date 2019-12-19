package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import org.openapitools.client.models.ApiResponse
import org.openapitools.client.models.Pet

interface PetApi {
    @POST("/pet")
    fun addPet(@Body body: Pet): Call<Unit>

    @DELETE("/pet/{petId}")
    fun deletePet(@Path("petId") petId: Long, @Header("api_key") apiKey: String): Call<Unit>

    @GET("/pet/findByStatus")
    fun findPetsByStatus(@Query("status") status: CSVParams): Call<Array<Pet>>

    @Deprecated("This api was deprecated")
    @GET("/pet/findByTags")
    fun findPetsByTags(@Query("tags") tags: CSVParams): Call<Array<Pet>>

    @GET("/pet/{petId}")
    fun getPetById(@Path("petId") petId: Long): Call<Pet>

    @PUT("/pet")
    fun updatePet(@Body body: Pet): Call<Unit>

    @FormUrlEncoded
    @POST("/pet/{petId}")
    fun updatePetWithForm(@Path("petId") petId: Long, @Field("name") name: String, @Field("status") status: String): Call<Unit>

    @Multipart
    @POST("/pet/{petId}/uploadImage")
    fun uploadFile(@Path("petId") petId: Long, @Part("additionalMetadata") additionalMetadata: String, @Part file: MultipartBody.Part ): Call<ApiResponse>

}
