package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import java.io.File;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
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

  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @throws ApiException if fails to make API call
   */
  public void addPet(Pet body) throws ApiException {
    Object localVarPostBody = body;
    
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling addPet");
    }
    
    // create path and map variables
    String localVarPath = "/pet".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json", "application/xml"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete (required)
   * @param apiKey  (optional)
   * @throws ApiException if fails to make API call
   */
  public void deletePet(Long petId, String apiKey) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
      throw new ApiException(400, "Missing the required parameter 'petId' when calling deletePet");
    }
    
    // create path and map variables
    String localVarPath = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    if (apiKey != null)
      localVarHeaderParams.put("api_key", apiClient.parameterToString(apiKey));

    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };


    apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for filter (required)
   * @return a {@code List<Pet>}
   * @throws ApiException if fails to make API call
   */
  public List<Pet> findPetsByStatus(List<String> status) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'status' is set
    if (status == null) {
      throw new ApiException(400, "Missing the required parameter 'status' when calling findPetsByStatus");
    }
    
    // create path and map variables
    String localVarPath = "/pet/findByStatus".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "status", status));

    
    
    
    final String[] localVarAccepts = {
      "application/xml", "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };

    GenericType<List<Pet>> localVarReturnType = new GenericType<List<Pet>>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Finds Pets by tags
   * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (required)
   * @return a {@code Set<Pet>}
   * @throws ApiException if fails to make API call
   * @deprecated
   */
  @Deprecated
  public Set<Pet> findPetsByTags(Set<String> tags) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'tags' is set
    if (tags == null) {
      throw new ApiException(400, "Missing the required parameter 'tags' when calling findPetsByTags");
    }
    
    // create path and map variables
    String localVarPath = "/pet/findByTags".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "tags", tags));

    
    
    
    final String[] localVarAccepts = {
      "application/xml", "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };

    GenericType<Set<Pet>> localVarReturnType = new GenericType<Set<Pet>>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Find pet by ID
   * Returns a single pet
   * @param petId ID of pet to return (required)
   * @return a {@code Pet}
   * @throws ApiException if fails to make API call
   */
  public Pet getPetById(Long petId) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
      throw new ApiException(400, "Missing the required parameter 'petId' when calling getPetById");
    }
    
    // create path and map variables
    String localVarPath = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "application/xml", "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "api_key" };

    GenericType<Pet> localVarReturnType = new GenericType<Pet>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (required)
   * @throws ApiException if fails to make API call
   */
  public void updatePet(Pet body) throws ApiException {
    Object localVarPostBody = body;
    
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling updatePet");
    }
    
    // create path and map variables
    String localVarPath = "/pet".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json", "application/xml"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };


    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated (required)
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @throws ApiException if fails to make API call
   */
  public void updatePetWithForm(Long petId, String name, String status) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
      throw new ApiException(400, "Missing the required parameter 'petId' when calling updatePetWithForm");
    }
    
    // create path and map variables
    String localVarPath = "/pet/{petId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    if (name != null)
      localVarFormParams.put("name", name);
if (status != null)
      localVarFormParams.put("status", status);

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return a {@code ModelApiResponse}
   * @throws ApiException if fails to make API call
   */
  public ModelApiResponse uploadFile(Long petId, String additionalMetadata, File file) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
      throw new ApiException(400, "Missing the required parameter 'petId' when calling uploadFile");
    }
    
    // create path and map variables
    String localVarPath = "/pet/{petId}/uploadImage".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    if (additionalMetadata != null)
      localVarFormParams.put("additionalMetadata", additionalMetadata);
if (file != null)
      localVarFormParams.put("file", file);

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "multipart/form-data"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };

    GenericType<ModelApiResponse> localVarReturnType = new GenericType<ModelApiResponse>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * uploads an image (required)
   * 
   * @param petId ID of pet to update (required)
   * @param requiredFile file to upload (required)
   * @param additionalMetadata Additional data to pass to server (optional)
   * @return a {@code ModelApiResponse}
   * @throws ApiException if fails to make API call
   */
  public ModelApiResponse uploadFileWithRequiredFile(Long petId, File requiredFile, String additionalMetadata) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'petId' is set
    if (petId == null) {
      throw new ApiException(400, "Missing the required parameter 'petId' when calling uploadFileWithRequiredFile");
    }
    
    // verify the required parameter 'requiredFile' is set
    if (requiredFile == null) {
      throw new ApiException(400, "Missing the required parameter 'requiredFile' when calling uploadFileWithRequiredFile");
    }
    
    // create path and map variables
    String localVarPath = "/fake/{petId}/uploadImageWithRequiredFile".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    if (additionalMetadata != null)
      localVarFormParams.put("additionalMetadata", additionalMetadata);
if (requiredFile != null)
      localVarFormParams.put("requiredFile", requiredFile);

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "multipart/form-data"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "petstore_auth" };

    GenericType<ModelApiResponse> localVarReturnType = new GenericType<ModelApiResponse>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
}
