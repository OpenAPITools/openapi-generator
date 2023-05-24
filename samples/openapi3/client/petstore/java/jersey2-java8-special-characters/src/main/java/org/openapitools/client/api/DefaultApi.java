package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import org.openapitools.client.model.MySchemaNameCharacters;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class DefaultApi {
  private ApiClient apiClient;

  public DefaultApi() {
    this(Configuration.getDefaultApiClient());
  }

  public DefaultApi(ApiClient apiClient) {
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
   * 
   * 
   * @param mySchemaNameCharacters  (optional)
   * @return MySchemaNameCharacters
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> the response </td><td>  -  </td></tr>
     </table>
   */
  public MySchemaNameCharacters testPost(MySchemaNameCharacters mySchemaNameCharacters) throws ApiException {
    return testPostWithHttpInfo(mySchemaNameCharacters).getData();
  }

  /**
   * 
   * 
   * @param mySchemaNameCharacters  (optional)
   * @return ApiResponse&lt;MySchemaNameCharacters&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> the response </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<MySchemaNameCharacters> testPostWithHttpInfo(MySchemaNameCharacters mySchemaNameCharacters) throws ApiException {
    String localVarAccept = apiClient.selectHeaderAccept("application/json");
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    GenericType<MySchemaNameCharacters> localVarReturnType = new GenericType<MySchemaNameCharacters>() {};
    return apiClient.invokeAPI("DefaultApi.testPost", "/test", "POST", new ArrayList<>(), mySchemaNameCharacters,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, localVarReturnType, false);
  }
}
