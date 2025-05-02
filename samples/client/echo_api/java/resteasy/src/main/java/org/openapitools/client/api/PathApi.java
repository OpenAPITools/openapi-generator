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

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class PathApi {
  private ApiClient apiClient;

  public PathApi() {
    this(Configuration.getDefaultApiClient());
  }

  public PathApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Test path parameter(s)
   * Test path parameter(s)
   * @param pathString  (required)
   * @param pathInteger  (required)
   * @param enumNonrefStringPath  (required)
   * @param enumRefStringPath  (required)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(@javax.annotation.Nonnull String pathString, @javax.annotation.Nonnull Integer pathInteger, @javax.annotation.Nonnull String enumNonrefStringPath, @javax.annotation.Nonnull StringEnumRef enumRefStringPath) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'pathString' is set
    if (pathString == null) {
      throw new ApiException(400, "Missing the required parameter 'pathString' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
    }
    
    // verify the required parameter 'pathInteger' is set
    if (pathInteger == null) {
      throw new ApiException(400, "Missing the required parameter 'pathInteger' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
    }
    
    // verify the required parameter 'enumNonrefStringPath' is set
    if (enumNonrefStringPath == null) {
      throw new ApiException(400, "Missing the required parameter 'enumNonrefStringPath' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
    }
    
    // verify the required parameter 'enumRefStringPath' is set
    if (enumRefStringPath == null) {
      throw new ApiException(400, "Missing the required parameter 'enumRefStringPath' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
    }
    
    // create path and map variables
    String localVarPath = "/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "path_string" + "\\}", apiClient.escapeString(pathString.toString()))
      .replaceAll("\\{" + "path_integer" + "\\}", apiClient.escapeString(pathInteger.toString()))
      .replaceAll("\\{" + "enum_nonref_string_path" + "\\}", apiClient.escapeString(enumNonrefStringPath.toString()))
      .replaceAll("\\{" + "enum_ref_string_path" + "\\}", apiClient.escapeString(enumRefStringPath.toString()));

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
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
