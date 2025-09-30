package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import org.openapitools.client.model.TestFormObjectMultipartRequestMarker;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class FormApi {
  private ApiClient apiClient;

  public FormApi() {
    this(Configuration.getDefaultApiClient());
  }

  public FormApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Test form parameter(s)
   * Test form parameter(s)
   * @param integerForm  (optional)
   * @param booleanForm  (optional)
   * @param stringForm  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testFormIntegerBooleanString(@javax.annotation.Nullable Integer integerForm, @javax.annotation.Nullable Boolean booleanForm, @javax.annotation.Nullable String stringForm) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/form/integer/boolean/string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    if (integerForm != null)
      localVarFormParams.put("integer_form", integerForm);
if (booleanForm != null)
      localVarFormParams.put("boolean_form", booleanForm);
if (stringForm != null)
      localVarFormParams.put("string_form", stringForm);

    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test form parameter(s) for multipart schema
   * Test form parameter(s) for multipart schema
   * @param marker  (required)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testFormObjectMultipart(@javax.annotation.Nonnull TestFormObjectMultipartRequestMarker marker) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'marker' is set
    if (marker == null) {
      throw new ApiException(400, "Missing the required parameter 'marker' when calling testFormObjectMultipart");
    }
    
    // create path and map variables
    String localVarPath = "/form/object/multipart".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    if (marker != null)
      localVarFormParams.put("marker", marker);

    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "multipart/form-data"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Test form parameter(s) for oneOf schema
   * Test form parameter(s) for oneOf schema
   * @param form1  (optional)
   * @param form2  (optional)
   * @param form3  (optional)
   * @param form4  (optional)
   * @param id  (optional)
   * @param name  (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String testFormOneof(@javax.annotation.Nullable String form1, @javax.annotation.Nullable Integer form2, @javax.annotation.Nullable String form3, @javax.annotation.Nullable Boolean form4, @javax.annotation.Nullable Long id, @javax.annotation.Nullable String name) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/form/oneof".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    if (form1 != null)
      localVarFormParams.put("form1", form1);
if (form2 != null)
      localVarFormParams.put("form2", form2);
if (form3 != null)
      localVarFormParams.put("form3", form3);
if (form4 != null)
      localVarFormParams.put("form4", form4);
if (id != null)
      localVarFormParams.put("id", id);
if (name != null)
      localVarFormParams.put("name", name);

    final String[] localVarAccepts = {
      "text/plain"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
}
