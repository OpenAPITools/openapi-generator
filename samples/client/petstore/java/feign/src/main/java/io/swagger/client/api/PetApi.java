package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;
import io.swagger.client.TypeRef;

import io.swagger.client.model.Pet;
import java.io.File;
import io.swagger.client.model.ApiResponse;


import java.util.*;

import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-12-07T01:11:21.159-05:00")
public interface PetApi extends io.swagger.client.ApiClient.Api {


  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store
   * @return void
   */
  @RequestLine("PUT /pet")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void updatePet(Pet body) throws ApiException;
  
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store
   * @return void
   */
  @RequestLine("POST /pet")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void addPet(Pet body) throws ApiException;
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @return List<Pet>
   */
  @RequestLine("GET /pet/findByStatus?status={status}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  List<Pet> findPetsByStatus(@Param("status") List<String> status) throws ApiException;
  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @return List<Pet>
   */
  @RequestLine("GET /pet/findByTags?tags={tags}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  List<Pet> findPetsByTags(@Param("tags") List<String> tags) throws ApiException;
  
  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return
   * @return Pet
   */
  @RequestLine("GET /pet/{petId}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  Pet getPetById(@Param("petId") Long petId) throws ApiException;
  
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @return void
   */
  @RequestLine("POST /pet/{petId}")
  @Headers({
    "Content-type: application/x-www-form-urlencoded",
    "Accepts: application/json",
  })
  void updatePetWithForm(@Param("petId") Long petId, @Param("name") String name, @Param("status") String status) throws ApiException;
  
  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete
   * @param apiKey 
   * @return void
   */
  @RequestLine("DELETE /pet/{petId}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
    "apiKey: {apiKey}"
  })
  void deletePet(@Param("petId") Long petId, @Param("apiKey") String apiKey) throws ApiException;
  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @return ApiResponse
   */
  @RequestLine("POST /pet/{petId}/uploadImage")
  @Headers({
    "Content-type: multipart/form-data",
    "Accepts: application/json",
  })
  ApiResponse uploadFile(@Param("petId") Long petId, @Param("additionalMetadata") String additionalMetadata, @Param("file") File file) throws ApiException;
  

}
