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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.*;
import retrofit2.Response;

public interface PetApi {
  /**
   * Add a new pet to the store
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("pet")
  CompletionStage<Response<Void>> addPet(
    @retrofit2.http.Body Pet pet
  );

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Call&lt;Void&gt;
   */
  @DELETE("pet/{petId}")
  CompletionStage<Response<Void>> deletePet(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Header("api_key") String apiKey
  );

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return Call&lt;List&lt;Pet&gt;&gt;
   */
  @GET("pet/findByStatus")
  CompletionStage<Response<List<Pet>>> findPetsByStatus(
    @retrofit2.http.Query("status") CSVParams status
  );

  /**
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return Call&lt;List&lt;Pet&gt;&gt;
   */
  @GET("pet/findByTags")
  CompletionStage<Response<List<Pet>>> findPetsByTags(
    @retrofit2.http.Query("tags") CSVParams tags
  );

  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Call&lt;Pet&gt;
   */
  @GET("pet/{petId}")
  CompletionStage<Response<Pet>> getPetById(
    @retrofit2.http.Path("petId") Long petId
  );

  /**
   * Update an existing pet
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("pet")
  CompletionStage<Response<Void>> updatePet(
    @retrofit2.http.Body Pet pet
  );

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional, default to null)
   * @param status Updated status of the pet (optional, default to null)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("pet/{petId}")
  CompletionStage<Response<Void>> updatePetWithForm(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Field("name") String name, @retrofit2.http.Field("status") String status
  );

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional, default to null)
   * @param file file to upload (optional, default to null)
   * @return Call&lt;ModelApiResponse&gt;
   */
  @retrofit2.http.Multipart
  @POST("pet/{petId}/uploadImage")
  CompletionStage<Response<ModelApiResponse>> uploadFile(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Part("additionalMetadata") String additionalMetadata, @retrofit2.http.Part("file") MultipartBody.Part file
  );

  /**
   * uploads an image (required)
   * 
   * @param petId ID of pet to update (required)
   * @param file file to upload (required)
   * @param additionalMetadata Additional data to pass to server (optional, default to null)
   * @return Call&lt;ModelApiResponse&gt;
   */
  @retrofit2.http.Multipart
  @POST("fake/{petId}/uploadImageWithRequiredFile")
  CompletionStage<Response<ModelApiResponse>> uploadFileWithRequiredFile(
    @retrofit2.http.Path("petId") Long petId, @retrofit2.http.Part("file") MultipartBody.Part file, @retrofit2.http.Part("additionalMetadata") String additionalMetadata
  );

}
