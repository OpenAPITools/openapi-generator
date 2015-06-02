package io.swagger.client.api;

import io.swagger.client.model.*;

import retrofit.http.*;
import retrofit.mime.*;
import java.util.*;

import io.swagger.client.model.Pet;
import java.io.File;

public interface PetApi {
  
  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store
   * @return Void
   */
  
  @PUT("/pet")  
  Void updatePet(
    @Body Pet body
  );  
  
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store
   * @return Void
   */
  
  @POST("/pet")  
  Void addPet(
    @Body Pet body
  );  
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @return List<Pet>
   */
  
  @GET("/pet/findByStatus")  
  List<Pet> findPetsByStatus(
    @Query("status") List<String> status
  );  
  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @return List<Pet>
   */
  
  @GET("/pet/findByTags")  
  List<Pet> findPetsByTags(
    @Query("tags") List<String> tags
  );  
  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return Pet
   */
  
  @GET("/pet/{petId}")  
  Pet getPetById(
    @Path("petId") Long petId
  );  
  
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @return Void
   */
  
  @FormUrlEncoded
  @POST("/pet/{petId}")  
  Void updatePetWithForm(
    @Path("petId") String petId,@Field("name") String name,@Field("status") String status
  );  
  
  /**
   * Deletes a pet
   * 
   * @param apiKey 
   * @param petId Pet id to delete
   * @return Void
   */
  
  @DELETE("/pet/{petId}")  
  Void deletePet(
    @Header("api_key") String apiKey,@Path("petId") Long petId
  );  
  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @return Void
   */
  
  @Multipart
  @POST("/pet/{petId}/uploadImage")  
  Void uploadFile(
    @Path("petId") Long petId,@Part("additionalMetadata") String additionalMetadata,@Part("file") TypedFile file
  );  
  
}
