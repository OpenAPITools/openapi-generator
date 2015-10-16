package io.swagger.client.api;

import io.swagger.client.ApiCallback;
import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import com.google.gson.reflect.TypeToken;

import com.squareup.okhttp.Call;

import io.swagger.client.model.Pet;
import java.io.File;

import java.lang.reflect.Type;
import java.util.*;

public class PetApi {
  private ApiClient apiClient;

  public PetApi() {
    this(Configuration.getDefaultApiClient());
  }

  public PetApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /* Build call for updatePet */
  private Call updatePetCall(Pet body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      "application/json", "application/xml"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "PUT", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store
   */
  public void updatePet(Pet body) throws ApiException {
    Call call = updatePetCall(body);
    apiClient.execute(call);
  }

  /**
   * Update an existing pet (asynchronously)
   * 
   * @param body Pet object that needs to be added to the store
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call updatePetAsync(Pet body, ApiCallback<Void> callback) throws ApiException {
    Call call = updatePetCall(body);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for addPet */
  private Call addPetCall(Pet body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      "application/json", "application/xml"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store
   */
  public void addPet(Pet body) throws ApiException {
    Call call = addPetCall(body);
    apiClient.execute(call);
  }

  /**
   * Add a new pet to the store (asynchronously)
   * 
   * @param body Pet object that needs to be added to the store
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call addPetAsync(Pet body, ApiCallback<Void> callback) throws ApiException {
    Call call = addPetCall(body);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for findPetsByStatus */
  private Call findPetsByStatusCall(List<String> status) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByStatus".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();
    if (status != null)
      queryParams.addAll(apiClient.parameterToPairs("multi", "status", status));

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @return List<Pet>
   */
  public List<Pet> findPetsByStatus(List<String> status) throws ApiException {
    Call call = findPetsByStatusCall(status);
    Type returnType = new TypeToken<List<Pet>>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Finds Pets by status (asynchronously)
   * Multiple status values can be provided with comma seperated strings
   * @param status Status values that need to be considered for filter
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call findPetsByStatusAsync(List<String> status, ApiCallback<List<Pet>> callback) throws ApiException {
    Call call = findPetsByStatusCall(status);
    Type returnType = new TypeToken<List<Pet>>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for findPetsByTags */
  private Call findPetsByTagsCall(List<String> tags) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByTags".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();
    if (tags != null)
      queryParams.addAll(apiClient.parameterToPairs("multi", "tags", tags));

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @return List<Pet>
   */
  public List<Pet> findPetsByTags(List<String> tags) throws ApiException {
    Call call = findPetsByTagsCall(tags);
    Type returnType = new TypeToken<List<Pet>>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Finds Pets by tags (asynchronously)
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call findPetsByTagsAsync(List<String> tags, ApiCallback<List<Pet>> callback) throws ApiException {
    Call call = findPetsByTagsCall(tags);
    Type returnType = new TypeToken<List<Pet>>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for getPetById */
  private Call getPetByIdCall(Long petId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException("Missing the required parameter 'petId' when calling getPetById(Async)");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "api_key" };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @return Pet
   */
  public Pet getPetById(Long petId) throws ApiException {
    Call call = getPetByIdCall(petId);
    Type returnType = new TypeToken<Pet>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Find pet by ID (asynchronously)
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call getPetByIdAsync(Long petId, ApiCallback<Pet> callback) throws ApiException {
    Call call = getPetByIdCall(petId);
    Type returnType = new TypeToken<Pet>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for updatePetWithForm */
  private Call updatePetWithFormCall(String petId, String name, String status) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException("Missing the required parameter 'petId' when calling updatePetWithForm(Async)");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();
    if (name != null)
      formParams.put("name", name);
    if (status != null)
      formParams.put("status", status);

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   */
  public void updatePetWithForm(String petId, String name, String status) throws ApiException {
    Call call = updatePetWithFormCall(petId, name, status);
    apiClient.execute(call);
  }

  /**
   * Updates a pet in the store with form data (asynchronously)
   * 
   * @param petId ID of pet that needs to be updated
   * @param name Updated name of the pet
   * @param status Updated status of the pet
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call updatePetWithFormAsync(String petId, String name, String status, ApiCallback<Void> callback) throws ApiException {
    Call call = updatePetWithFormCall(petId, name, status);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for deletePet */
  private Call deletePetCall(Long petId, String apiKey) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException("Missing the required parameter 'petId' when calling deletePet(Async)");
    }
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();
    if (apiKey != null)
      headerParams.put("api_key", apiClient.parameterToString(apiKey));

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "DELETE", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete
   * @param apiKey 
   */
  public void deletePet(Long petId, String apiKey) throws ApiException {
    Call call = deletePetCall(petId, apiKey);
    apiClient.execute(call);
  }

  /**
   * Deletes a pet (asynchronously)
   * 
   * @param petId Pet id to delete
   * @param apiKey 
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call deletePetAsync(Long petId, String apiKey, ApiCallback<Void> callback) throws ApiException {
    Call call = deletePetCall(petId, apiKey);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for uploadFile */
  private Call uploadFileCall(Long petId, String additionalMetadata, File file) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
       throw new ApiException("Missing the required parameter 'petId' when calling uploadFile(Async)");
    }
    

    // create path and map variables
    String path = "/pet/{petId}/uploadImage".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();
    if (additionalMetadata != null)
      formParams.put("additionalMetadata", additionalMetadata);
    if (file != null)
      formParams.put("file", file);

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      "multipart/form-data"
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "petstore_auth" };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * uploads an image
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   */
  public void uploadFile(Long petId, String additionalMetadata, File file) throws ApiException {
    Call call = uploadFileCall(petId, additionalMetadata, file);
    apiClient.execute(call);
  }

  /**
   * uploads an image (asynchronously)
   * 
   * @param petId ID of pet to update
   * @param additionalMetadata Additional data to pass to server
   * @param file file to upload
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call uploadFileAsync(Long petId, String additionalMetadata, File file, ApiCallback<Void> callback) throws ApiException {
    Call call = uploadFileCall(petId, additionalMetadata, file);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
}
