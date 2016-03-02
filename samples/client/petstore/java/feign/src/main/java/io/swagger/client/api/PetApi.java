package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.Pet;
import java.io.File;
import io.swagger.client.model.PetWithArbitraryObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-03-02T21:04:56.888+08:00")
public interface PetApi extends ApiClient.Api {


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
  void updatePet(Pet body);
  
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
  void addPet(Pet body);
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for query
   * @return List<Pet>
   */
  @RequestLine("GET /pet/findByStatus?status={status}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  List<Pet> findPetsByStatus(@Param("status") List<String> status);
  
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
  List<Pet> findPetsByTags(@Param("tags") List<String> tags);
  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return Pet
   */
  @RequestLine("GET /pet/{petId}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  Pet getPetById(@Param("petId") Long petId);
  
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
  void updatePetWithForm(@Param("petId") String petId, @Param("name") String name, @Param("status") String status);
  
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
  void deletePet(@Param("petId") Long petId, @Param("apiKey") String apiKey);
  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @return void
   */
  @RequestLine("POST /pet/{petId}/uploadImage")
  @Headers({
    "Content-type: multipart/form-data",
    "Accepts: application/json",
  })
  void uploadFile(@Param("petId") Long petId, @Param("additionalMetadata") String additionalMetadata, @Param("file") File file);
  
  /**
   * Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return PetWithArbitraryObject
   */
  @RequestLine("GET /pet/{petId}?response=inline_arbitrary_object")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  PetWithArbitraryObject getPetByIdWithObject(@Param("petId") Long petId);
  
  /**
   * Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return byte[]
   */
  @RequestLine("GET /pet/{petId}?testing_byte_array=true")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  byte[] petPetIdtestingByteArraytrueGet(@Param("petId") Long petId);
  
  /**
   * Fake endpoint to test byte array in body parameter for adding a new pet to the store
   * 
   * @param body Pet object in the form of byte array
   * @return void
   */
  @RequestLine("POST /pet?testing_byte_array=true")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void addPetUsingByteArray(byte[] body);
  
}
