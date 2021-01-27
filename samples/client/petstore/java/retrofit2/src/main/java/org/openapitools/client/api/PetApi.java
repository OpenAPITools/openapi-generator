package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import java.io.File;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface PetApi {
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("pet")
  Call<Void> addPet(
    @retrofit2.http.Body Pet body
  );

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Call&lt;Void&gt;
   */
  @DELETE("pet/{petId}")
  Call<Void> deletePet(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Header("api_key") String apiKey
  );

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return Call&lt;List&lt;Pet&gt;&gt;
   */
  @GET("pet/findByStatus")
  Call<List<Pet>> findPetsByStatus(
    @retrofit2.http.Query("status") CSVParams status
  );

  /**
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return Call&lt;Set&lt;Pet&gt;&gt;
   * @deprecated
   */
  @Deprecated
  @GET("pet/findByTags")
  Call<Set<Pet>> findPetsByTags(
    @retrofit2.http.Query("tags") CSVParams tags
  );

  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Call&lt;Pet&gt;
   */
  @GET("pet/{petId}")
  Call<Pet> getPetById(
    @retrofit2.http.Path("petId") Long petId
  );

  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("pet")
  Call<Void> updatePet(
    @retrofit2.http.Body Pet body
  );

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("pet/{petId}")
  Call<Void> updatePetWithForm(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Field("name") String name, @retrofit2.http.Field("status") String status
  );

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return Call&lt;ModelApiResponse&gt;
   */
  @retrofit2.http.Multipart
  @POST("pet/{petId}/uploadImage")
  Call<ModelApiResponse> uploadFile(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Part("additionalMetadata") String additionalMetadata, @retrofit2.http.Part MultipartBody.Part file
  );

  /**
   * uploads an image (required)
   * 
   * @param petId ID of pet to update (required)
   * @param requiredFile file to upload (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @return Call&lt;ModelApiResponse&gt;
   */
  @retrofit2.http.Multipart
  @POST("fake/{petId}/uploadImageWithRequiredFile")
  Call<ModelApiResponse> uploadFileWithRequiredFile(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Part MultipartBody.Part requiredFile, @retrofit2.http.Part("additionalMetadata") String additionalMetadata
  );

}
