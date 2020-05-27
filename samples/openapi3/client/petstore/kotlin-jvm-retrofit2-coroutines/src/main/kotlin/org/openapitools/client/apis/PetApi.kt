package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import org.openapitools.client.models.ApiResponse
import org.openapitools.client.models.Pet

interface PetApi {

    @POST("pet")
    suspend fun addPet(@Body pet: Pet): Unit

    @DELETE("pet/{petId}")
    suspend fun deletePet(@Path("petId") petId: kotlin.Long, @Header("api_key") apiKey: kotlin.String? = null): Unit

    @GET("pet/findByStatus")
    suspend fun findPetsByStatus(@Query("status") status: CSVParams): kotlin.collections.List<Pet>

    @Deprecated("This api was deprecated")
    @GET("pet/findByTags")
    suspend fun findPetsByTags(@Query("tags") tags: CSVParams): kotlin.collections.List<Pet>

    @GET("pet/{petId}")
    suspend fun getPetById(@Path("petId") petId: kotlin.Long): Pet

    @PUT("pet")
    suspend fun updatePet(@Body pet: Pet): Unit

    @FormUrlEncoded
    @POST("pet/{petId}")
    suspend fun updatePetWithForm(@Path("petId") petId: kotlin.Long, @Field("name") name: kotlin.String? = null, @Field("status") status: kotlin.String? = null): Unit

    @Multipart
    @POST("pet/{petId}/uploadImage")
    suspend fun uploadFile(@Path("petId") petId: kotlin.Long, @Part("additionalMetadata") additionalMetadata: kotlin.String? = null, @Part file: MultipartBody.Part? = null): ApiResponse

    @Multipart
    @POST("fake/{petId}/uploadImageWithRequiredFile")
    suspend fun uploadFileWithRequiredFile(@Path("petId") petId: kotlin.Long, @Part requiredFile: MultipartBody.Part, @Part("additionalMetadata") additionalMetadata: kotlin.String? = null): ApiResponse

}
