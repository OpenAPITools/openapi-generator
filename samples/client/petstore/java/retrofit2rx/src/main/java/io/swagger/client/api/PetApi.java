package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import rx.Observable;

import retrofit2.http.*;

import okhttp3.RequestBody;

import io.swagger.client.model.Pet;
import io.swagger.client.model.ModelApiResponse;
import java.io.File;

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
  
  @POST("pet")
  Observable<Void> addPet(
    @Body Pet body
  );

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @return Call&lt;Void&gt;
   */
  
  @DELETE("pet/{petId}")
  Observable<Void> deletePet(
    @Path("petId") Long petId, @Header("api_key") String apiKey
  );

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return Call&lt;List<Pet>&gt;
   */
  
  @GET("pet/findByStatus")
  Observable<List<Pet>> findPetsByStatus(
    @Query("status") CSVParams status
  );

  /**
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return Call&lt;List<Pet>&gt;
   */
  
  @GET("pet/findByTags")
  Observable<List<Pet>> findPetsByTags(
    @Query("tags") CSVParams tags
  );

  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return Call&lt;Pet&gt;
   */
  
  @GET("pet/{petId}")
  Observable<Pet> getPetById(
    @Path("petId") Long petId
  );

  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @return Call&lt;Void&gt;
   */
  
  @PUT("pet")
  Observable<Void> updatePet(
    @Body Pet body
  );

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return Call&lt;Void&gt;
   */
  
  @FormUrlEncoded
  @POST("pet/{petId}")
  Observable<Void> updatePetWithForm(
    @Path("petId") Long petId, @Field("name") String name, @Field("status") String status
  );

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return Call&lt;ModelApiResponse&gt;
   */
  
  @Multipart
  @POST("pet/{petId}/uploadImage")
  Observable<ModelApiResponse> uploadFile(
    @Path("petId") Long petId, @Part("additionalMetadata") String additionalMetadata, @Part("file\"; filename=\"file") RequestBody file
  );

}
