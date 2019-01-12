package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import java.io.File;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface PetApi {
  /**
   * Add a new pet to the store
   * Sync method
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Void
   */
  
  @POST("/pet")
  Void addPet(
    @retrofit.http.Body Pet body
  );

  /**
   * Add a new pet to the store
   * Async method
   * @param body Pet object that needs to be added to the store (required)
   * @param cb callback method
   */
  
  @POST("/pet")
  void addPet(
    @retrofit.http.Body Pet body, Callback<Void> cb
  );
  /**
   * Deletes a pet
   * Sync method
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Void
   */
  
  @DELETE("/pet/{petId}")
  Void deletePet(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Header("api_key") String apiKey
  );

  /**
   * Deletes a pet
   * Async method
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @param cb callback method
   */
  
  @DELETE("/pet/{petId}")
  void deletePet(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Header("api_key") String apiKey, Callback<Void> cb
  );
  /**
   * Finds Pets by status
   * Sync method
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return List&lt;Pet&gt;
   */
  
  @GET("/pet/findByStatus")
  List<Pet> findPetsByStatus(
    @retrofit.http.Query("status") CSVParams status
  );

  /**
   * Finds Pets by status
   * Async method
   * @param status Status values that need to be considered for filter (required)
   * @param cb callback method
   */
  
  @GET("/pet/findByStatus")
  void findPetsByStatus(
    @retrofit.http.Query("status") CSVParams status, Callback<List<Pet>> cb
  );
  /**
   * Finds Pets by tags
   * Sync method
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return List&lt;Pet&gt;
   */
  
  @GET("/pet/findByTags")
  List<Pet> findPetsByTags(
    @retrofit.http.Query("tags") CSVParams tags
  );

  /**
   * Finds Pets by tags
   * Async method
   * @param tags Tags to filter by (required)
   * @param cb callback method
   */
  
  @GET("/pet/findByTags")
  void findPetsByTags(
    @retrofit.http.Query("tags") CSVParams tags, Callback<List<Pet>> cb
  );
  /**
   * Find pet by ID
   * Sync method
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Pet
   */
  
  @GET("/pet/{petId}")
  Pet getPetById(
    @retrofit.http.Path("petId") Long petId
  );

  /**
   * Find pet by ID
   * Async method
   * @param petId ID of pet to return (required)
   * @param cb callback method
   */
  
  @GET("/pet/{petId}")
  void getPetById(
    @retrofit.http.Path("petId") Long petId, Callback<Pet> cb
  );
  /**
   * Update an existing pet
   * Sync method
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Void
   */
  
  @PUT("/pet")
  Void updatePet(
    @retrofit.http.Body Pet body
  );

  /**
   * Update an existing pet
   * Async method
   * @param body Pet object that needs to be added to the store (required)
   * @param cb callback method
   */
  
  @PUT("/pet")
  void updatePet(
    @retrofit.http.Body Pet body, Callback<Void> cb
  );
  /**
   * Updates a pet in the store with form data
   * Sync method
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return Void
   */
  
  @retrofit.http.FormUrlEncoded
  @POST("/pet/{petId}")
  Void updatePetWithForm(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status
  );

  /**
   * Updates a pet in the store with form data
   * Async method
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @param cb callback method
   */
  
  @retrofit.http.FormUrlEncoded
  @POST("/pet/{petId}")
  void updatePetWithForm(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Field("name") String name, @retrofit.http.Field("status") String status, Callback<Void> cb
  );
  /**
   * uploads an image
   * Sync method
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return ModelApiResponse
   */
  
  @retrofit.http.Multipart
  @POST("/pet/{petId}/uploadImage")
  ModelApiResponse uploadFile(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Part("additionalMetadata") String additionalMetadata, @retrofit.http.Part("file") TypedFile file
  );

  /**
   * uploads an image
   * Async method
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @param cb callback method
   */
  
  @retrofit.http.Multipart
  @POST("/pet/{petId}/uploadImage")
  void uploadFile(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Part("additionalMetadata") String additionalMetadata, @retrofit.http.Part("file") TypedFile file, Callback<ModelApiResponse> cb
  );
  /**
   * uploads an image (required)
   * Sync method
   * 
   * @param petId ID of pet to update (required)
   * @param requiredFile file to upload (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @return ModelApiResponse
   */
  
  @retrofit.http.Multipart
  @POST("/fake/{petId}/uploadImageWithRequiredFile")
  ModelApiResponse uploadFileWithRequiredFile(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Part("requiredFile") TypedFile requiredFile, @retrofit.http.Part("additionalMetadata") String additionalMetadata
  );

  /**
   * uploads an image (required)
   * Async method
   * @param petId ID of pet to update (required)
   * @param requiredFile file to upload (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param cb callback method
   */
  
  @retrofit.http.Multipart
  @POST("/fake/{petId}/uploadImageWithRequiredFile")
  void uploadFileWithRequiredFile(
    @retrofit.http.Path("petId") Long petId, @retrofit.http.Part("requiredFile") TypedFile requiredFile, @retrofit.http.Part("additionalMetadata") String additionalMetadata, Callback<ModelApiResponse> cb
  );
}
