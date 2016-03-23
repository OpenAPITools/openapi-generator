package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;


import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;

import io.swagger.client.model.Pet;
import io.swagger.client.model.InlineResponse200;
import java.io.File;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface PetApi {
  
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (optional)
   * @return Call<Void>
   */
  
  @POST("pet")
  Call<Void> addPet(
    @Body Pet body
  );

  
  /**
   * Fake endpoint to test byte array in body parameter for adding a new pet to the store
   * 
   * @param body Pet object in the form of byte array (optional)
   * @return Call<Void>
   */
  
  @POST("pet?testing_byte_array=true")
  Call<Void> addPetUsingByteArray(
    @Body byte[] body
  );

  
  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Call<Void>
   */
  
  @DELETE("pet/{petId}")
  Call<Void> deletePet(
    @Path("petId") Long petId, @Header("api_key") String apiKey
  );

  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for query (optional, default to available)
   * @return Call<List<Pet>>
   */
  
  @GET("pet/findByStatus")
  Call<List<Pet>> findPetsByStatus(
    @Query("status") List<String> status
  );

  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (optional)
   * @return Call<List<Pet>>
   */
  
  @GET("pet/findByTags")
  Call<List<Pet>> findPetsByTags(
    @Query("tags") List<String> tags
  );

  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched (required)
   * @return Call<Pet>
   */
  
  @GET("pet/{petId}")
  Call<Pet> getPetById(
    @Path("petId") Long petId
  );

  
  /**
   * Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched (required)
   * @return Call<InlineResponse200>
   */
  
  @GET("pet/{petId}?response=inline_arbitrary_object")
  Call<InlineResponse200> getPetByIdInObject(
    @Path("petId") Long petId
  );

  
  /**
   * Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched (required)
   * @return Call<byte[]>
   */
  
  @GET("pet/{petId}?testing_byte_array=true")
  Call<byte[]> petPetIdtestingByteArraytrueGet(
    @Path("petId") Long petId
  );

  
  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (optional)
   * @return Call<Void>
   */
  
  @PUT("pet")
  Call<Void> updatePet(
    @Body Pet body
  );

  
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return Call<Void>
   */
  
  @FormUrlEncoded
  @POST("pet/{petId}")
  Call<Void> updatePetWithForm(
    @Path("petId") String petId, @Field("name") String name, @Field("status") String status
  );

  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return Call<Void>
   */
  
  @Multipart
  @POST("pet/{petId}/uploadImage")
  Call<Void> uploadFile(
    @Path("petId") Long petId, @Part("additionalMetadata") String additionalMetadata, @Part("file\"; filename=\"file\"") RequestBody file
  );

  
}
