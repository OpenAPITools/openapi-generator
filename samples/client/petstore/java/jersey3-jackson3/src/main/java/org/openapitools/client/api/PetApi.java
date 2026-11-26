package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import jakarta.ws.rs.core.GenericType;

import org.openapitools.client.model.Pet;

import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.29.0-SNAPSHOT")
public class PetApi {
  private ApiClient apiClient;

  public PetApi() {
    this(Configuration.getDefaultApiClient());
  }

  public PetApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Get the API client
   *
   * @return API client
   */
  public ApiClient getApiClient() {
    return apiClient;
  }

  /**
   * Set the API client
   *
   * @param apiClient an instance of API client
   */
  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Add a new pet to the store
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @return Pet
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table border="1">
       <caption>Response Details</caption>
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 405 </td><td> Invalid input </td><td>  -  </td></tr>
     </table>
   */
  public Pet addPet(@jakarta.annotation.Nonnull Pet pet) throws ApiException {
    return addPetWithHttpInfo(pet).getData();
  }

  /**
   * Add a new pet to the store
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @return ApiResponse&lt;Pet&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table border="1">
       <caption>Response Details</caption>
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 405 </td><td> Invalid input </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Pet> addPetWithHttpInfo(@jakarta.annotation.Nonnull Pet pet) throws ApiException {
    // Check required parameters
    if (pet == null) {
      throw new ApiException(400, "Missing the required parameter 'pet' when calling addPet");
    }

    String localVarAccept = apiClient.selectHeaderAccept("application/xml", "application/json");
    String localVarContentType = apiClient.selectHeaderContentType("application/json", "application/xml");
    String[] localVarAuthNames = new String[] {"petstore_auth"};
    final Map<String, GenericType> localVarErrorTypes = new HashMap<String, GenericType>();
    localVarErrorTypes.put("200", new GenericType<Pet>() {});
    GenericType<Pet> localVarReturnType = new GenericType<Pet>() {};
    return apiClient.invokeAPI("PetApi.addPet", "/pet", "POST", new ArrayList<>(), pet,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               localVarAuthNames, localVarReturnType, false, localVarErrorTypes);
  }
}
