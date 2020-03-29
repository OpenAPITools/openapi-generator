package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody
import io.reactivex.Single
import io.reactivex.Completable
import retrofit2.http.*

import org.openapitools.client.models.ApiResponse
import org.openapitools.client.models.Pet

interface PetApi {
    @POST("/pet")
    fun addPet(@Body pet: Pet): Completable

    @DELETE("/pet/{petId}")
    fun deletePet(@Path("petId") petId: kotlin.Long, @Header("api_key") apiKey: kotlin.String): Completable

    @GET("/pet/findByStatus")
    fun findPetsByStatus(@Query("status") status: CSVParams): Single<kotlin.Array<Pet>>

    @Deprecated("This api was deprecated")
    @GET("/pet/findByTags")
    fun findPetsByTags(@Query("tags") tags: CSVParams): Single<kotlin.Array<Pet>>

    @GET("/pet/{petId}")
    fun getPetById(@Path("petId") petId: kotlin.Long): Single<Pet>

    @PUT("/pet")
    fun updatePet(@Body pet: Pet): Completable

    @FormUrlEncoded
    @POST("/pet/{petId}")
    fun updatePetWithForm(@Path("petId") petId: kotlin.Long, @Field("name") name: kotlin.String, @Field("status") status: kotlin.String): Completable

    @Multipart
    @POST("/pet/{petId}/uploadImage")
    fun uploadFile(@Path("petId") petId: kotlin.Long, @Part("additionalMetadata") additionalMetadata: kotlin.String, @Part file: MultipartBody.Part ): Single<ApiResponse>

    @Multipart
    @POST("/fake/{petId}/uploadImageWithRequiredFile")
    fun uploadFileWithRequiredFile(@Path("petId") petId: kotlin.Long, @Part requiredFile: MultipartBody.Part , @Part("additionalMetadata") additionalMetadata: kotlin.String): Single<ApiResponse>

}
