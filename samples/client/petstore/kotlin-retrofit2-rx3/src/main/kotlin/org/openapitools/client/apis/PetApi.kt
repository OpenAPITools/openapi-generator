package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.core.Completable;

import org.openapitools.client.models.ModelApiResponse
import org.openapitools.client.models.Pet

import okhttp3.MultipartBody

interface PetApi {
    /**
     * Add a new pet to the store
     * 
     * Responses:
     *  - 405: Invalid input
     *
     * @param body Pet object that needs to be added to the store 
     * @return [Call]<[Unit]>
     */
    @POST("pet")
    fun addPet(@Body body: Pet): Completable

    /**
     * Deletes a pet
     * 
     * Responses:
     *  - 400: Invalid pet value
     *
     * @param petId Pet id to delete 
     * @param apiKey  (optional)
     * @return [Call]<[Unit]>
     */
    @DELETE("pet/{petId}")
    fun deletePet(@Path("petId") petId: kotlin.Long, @Header("api_key") apiKey: kotlin.String? = null): Completable

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid status value
     *
     * @param status Status values that need to be considered for filter 
     * @return [Call]<[kotlin.collections.List<Pet>]>
     */
    @GET("pet/findByStatus")
    fun findPetsByStatus(@Query("status") status: CSVParams): Single<kotlin.collections.List<Pet>>

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid tag value
     *
     * @param tags Tags to filter by 
     * @return [Call]<[kotlin.collections.List<Pet>]>
     */
    @Deprecated("This api was deprecated")
    @GET("pet/findByTags")
    fun findPetsByTags(@Query("tags") tags: CSVParams): Single<kotlin.collections.List<Pet>>

    /**
     * Find pet by ID
     * Returns a single pet
     * Responses:
     *  - 200: successful operation
     *  - 400: Invalid ID supplied
     *  - 404: Pet not found
     *
     * @param petId ID of pet to return 
     * @return [Call]<[Pet]>
     */
    @GET("pet/{petId}")
    fun getPetById(@Path("petId") petId: kotlin.Long): Single<Pet>

    /**
     * Update an existing pet
     * 
     * Responses:
     *  - 400: Invalid ID supplied
     *  - 404: Pet not found
     *  - 405: Validation exception
     *
     * @param body Pet object that needs to be added to the store 
     * @return [Call]<[Unit]>
     */
    @PUT("pet")
    fun updatePet(@Body body: Pet): Completable

    /**
     * Updates a pet in the store with form data
     * 
     * Responses:
     *  - 405: Invalid input
     *
     * @param petId ID of pet that needs to be updated 
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return [Call]<[Unit]>
     */
    @FormUrlEncoded
    @POST("pet/{petId}")
    fun updatePetWithForm(@Path("petId") petId: kotlin.Long, @Field("name") name: kotlin.String? = null, @Field("status") status: kotlin.String? = null): Completable

    /**
     * uploads an image
     * 
     * Responses:
     *  - 200: successful operation
     *
     * @param petId ID of pet to update 
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @return [Call]<[ModelApiResponse]>
     */
    @Multipart
    @POST("pet/{petId}/uploadImage")
    fun uploadFile(@Path("petId") petId: kotlin.Long, @Part("additionalMetadata") additionalMetadata: kotlin.String? = null, @Part file: MultipartBody.Part? = null): Single<ModelApiResponse>

}
