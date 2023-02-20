package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import org.openapitools.client.model.Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class FakeClassnameTags123Api {
  private ApiClient apiClient;

  public FakeClassnameTags123Api() {
    this(Configuration.getDefaultApiClient());
  }

  public FakeClassnameTags123Api(ApiClient apiClient) {
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
   * To test class name in snake case
   * To test class name in snake case
   * @param client client model (required)
   * @return Client
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public Client testClassname(Client client) throws ApiException {
    return testClassnameWithHttpInfo(client).getData();
  }

  /**
   * To test class name in snake case
   * To test class name in snake case
   * @param client client model (required)
   * @return ApiResponse&lt;Client&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Client> testClassnameWithHttpInfo(Client client) throws ApiException {
    // Check required parameters
    if (client == null) {
      throw new ApiException(400, "Missing the required parameter 'client' when calling testClassname");
    }

    String localVarAccept = apiClient.selectHeaderAccept("application/json");
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    String[] localVarAuthNames = new String[] {"api_key_query"};
    GenericType<Client> localVarReturnType = new GenericType<Client>() {};
    return apiClient.invokeAPI("FakeClassnameTags123Api.testClassname", "/fake_classname_test", "PATCH", new ArrayList<>(), client,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               localVarAuthNames, localVarReturnType, false);
  }
}
