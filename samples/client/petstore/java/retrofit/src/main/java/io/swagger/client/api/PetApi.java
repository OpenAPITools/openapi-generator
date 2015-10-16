package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Pet;
import java.io.File;

import java.util.*;

public interface PetApi {
  
  /**
   * Update an existing pet
   * Sync method
   * 
   * @param body Pet object that needs to be added to the store
   * @return Void
   */
  
  @PUT("/pet")
  Void updatePet(
    @Body Pet body
  );

  /**
   * Update an existing pet
   * Async method
   * @param body Pet object that needs to be added to the store
   * @param cb callback method
   * @return void
   */
  
  @PUT("/pet")
  void updatePet(
    @Body Pet body, Callback<Void> cb
  );
  
  /**
   * Add a new pet to the store
   * Sync method
   * 
   * @param body Pet object that needs to be added to the store
   * @return Void
   */
  
  @POST("/pet")
  Void addPet(
    @Body Pet body
  );

  /**
   * Add a new pet to the store
   * Async method
   * @param body Pet object that needs to be added to the store
   * @param cb callback method
   * @return void
   */
  
  @POST("/pet")
  void addPet(
    @Body Pet body, Callback<Void> cb
  );
  
  /**
   * Finds Pets by status
   * Sync method
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @return List<Pet>
   */
  
  @GET("/pet/findByStatus")
  List<Pet> findPetsByStatus(
    @Query("status") List<String> status
  );

  /**
   * Finds Pets by status
   * Async method
   * @param status Status values that need to be considered for filter
   * @param cb callback method
   * @return void
   */
  
  @GET("/pet/findByStatus")
  void findPetsByStatus(
    @Query("status") List<String> status, Callback<List<Pet>> cb
  );
  
  /**
   * Finds Pets by tags
   * Sync method
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @return List<Pet>
   */
  
  @GET("/pet/findByTags")
  List<Pet> findPetsByTags(
    @Query("tags") List<String> tags
  );

  /**
   * Finds Pets by tags
   * Async method
   * @param tags Tags to filter by
   * @param cb callback method
   * @return void
   */
  
  @GET("/pet/findByTags")
  void findPetsByTags(
    @Query("tags") List<String> tags, Callback<List<Pet>> cb
  );
  
  /**
   * Find pet by ID
   * Sync method
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return Pet
   */
  
  @GET("/pet/{petId}")
  Pet getPetById(
    @Path("petId") Long petId
  );

  /**
   * Find pet by ID
   * Async method
   * @param petId ID of pet that needs to be fetched
   * @param cb callback method
   * @return void
   */
  
  @GET("/pet/{petId}")
  void getPetById(
    @Path("petId") Long petId, Callback<Pet> cb
  );
  
  /**
   * Updates a pet in the store with form data
   * Sync method
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @return Void
   */
  
  @FormUrlEncoded
  @POST("/pet/{petId}")
  Void updatePetWithForm(
    @Path("petId") String petId, @Field("name") String name, @Field("status") String status
  );

  /**
   * Updates a pet in the store with form data
   * Async method
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @param cb callback method
   * @return void
   */
  
  @FormUrlEncoded
  @POST("/pet/{petId}")
  void updatePetWithForm(
    @Path("petId") String petId, @Field("name") String name, @Field("status") String status, Callback<Void> cb
  );
  
  /**
   * Deletes a pet
   * Sync method
   * 
   * @param petId Pet id to delete
   * @param apiKey 
   * @return Void
   */
  
  @DELETE("/pet/{petId}")
  Void deletePet(
    @Path("petId") Long petId, @Header("api_key") String apiKey
  );

  /**
   * Deletes a pet
   * Async method
   * @param petId Pet id to delete
   * @param apiKey 
   * @param cb callback method
   * @return void
   */
  
  @DELETE("/pet/{petId}")
  void deletePet(
    @Path("petId") Long petId, @Header("api_key") String apiKey, Callback<Void> cb
  );
  
  /**
   * uploads an image
   * Sync method
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @return Void
   */
  
  @Multipart
  @POST("/pet/{petId}/uploadImage")
  Void uploadFile(
    @Path("petId") Long petId, @Part("additionalMetadata") String additionalMetadata, @Part("file") TypedFile file
  );

  /**
   * uploads an image
   * Async method
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @param cb callback method
   * @return void
   */
  
  @Multipart
  @POST("/pet/{petId}/uploadImage")
  void uploadFile(
    @Path("petId") Long petId, @Part("additionalMetadata") String additionalMetadata, @Part("file") TypedFile file, Callback<Void> cb
  );
  
}
