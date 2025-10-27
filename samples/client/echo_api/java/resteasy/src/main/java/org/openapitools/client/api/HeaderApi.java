package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import org.openapitools.client.model.StringEnumRef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class HeaderApi {
  private ApiClient apiClient;

  public HeaderApi() {
    this(Configuration.getDefaultApiClient());
  }

  public HeaderApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Test header parameter(s)
   * Test header parameter(s)
   * @param integerHeader  (optional)
   * @param booleanHeader  (optional)
   * @param stringHeader  (optional)
   * @param enumNonrefStringHeader  (optional)
   * @param enumRefStringHeader  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testHeaderIntegerBooleanStringEnums(@javax.annotation.Nullable Integer integerHeader, @javax.annotation.Nullable Boolean booleanHeader, @javax.annotation.Nullable String stringHeader, @javax.annotation.Nullable String enumNonrefStringHeader, @javax.annotation.Nullable StringEnumRef enumRefStringHeader) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/header/integer/boolean/string/enums".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    if (integerHeader != null)
      localVarHeaderParams.put("integer_header", apiClient.parameterToString(integerHeader));
if (booleanHeader != null)
      localVarHeaderParams.put("boolean_header", apiClient.parameterToString(booleanHeader));
if (stringHeader != null)
      localVarHeaderParams.put("string_header", apiClient.parameterToString(stringHeader));
if (enumNonrefStringHeader != null)
      localVarHeaderParams.put("enum_nonref_string_header", apiClient.parameterToString(enumNonrefStringHeader));
if (enumRefStringHeader != null)
      localVarHeaderParams.put("enum_ref_string_header", apiClient.parameterToString(enumRefStringHeader));

    
    
    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
}
