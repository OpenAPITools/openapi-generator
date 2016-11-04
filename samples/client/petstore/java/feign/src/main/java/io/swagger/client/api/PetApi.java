package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.Pet;
import io.swagger.client.model.ModelApiResponse;
import java.io.File;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface PetApi extends ApiClient.Api {


  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return void
   */
  @RequestLine("POST /pet")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void addPet(Pet body);

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return void
   */
  @RequestLine("DELETE /pet/{petId}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
    "api_key: {apiKey}"
  })
  void deletePet(@Param("petId") Long petId, @Param("apiKey") String apiKey);

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return List<Pet>
   */
  @RequestLine("GET /pet/findByStatus?status={status}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  List<Pet> findPetsByStatus(@Param("status") List<String> status);

  /**
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return List<Pet>
   */
  @RequestLine("GET /pet/findByTags?tags={tags}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  List<Pet> findPetsByTags(@Param("tags") List<String> tags);

  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Pet
   */
  @RequestLine("GET /pet/{petId}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Pet getPetById(@Param("petId") Long petId);

  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return void
   */
  @RequestLine("PUT /pet")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void updatePet(Pet body);

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return void
   */
  @RequestLine("POST /pet/{petId}")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
  })
  void updatePetWithForm(@Param("petId") Long petId, @Param("name") String name, @Param("status") String status);

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return ModelApiResponse
   */
  @RequestLine("POST /pet/{petId}/uploadImage")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: application/json",
  })
  ModelApiResponse uploadFile(@Param("petId") Long petId, @Param("additionalMetadata") String additionalMetadata, @Param("file") File file);
}
