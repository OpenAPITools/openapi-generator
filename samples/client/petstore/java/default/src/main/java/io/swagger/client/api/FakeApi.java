package io.swagger.client.api;

import com.sun.jersey.api.client.GenericType;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import java.math.BigDecimal;
import java.util.Date;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class FakeApi {
  private ApiClient apiClient;

  public FakeApi() {
    this(Configuration.getDefaultApiClient());
  }

  public FakeApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Fake endpoint for testing various parameters
   * Fake endpoint for testing various parameters
   * @param number None (required)
   * @param _double None (required)
   * @param string None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParameters(BigDecimal number, Double _double, String string, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, byte[] binary, Date date, Date dateTime, String password) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'number' is set
    if (number == null) {
      throw new ApiException(400, "Missing the required parameter 'number' when calling testEndpointParameters");
    }
    
    // verify the required parameter '_double' is set
    if (_double == null) {
      throw new ApiException(400, "Missing the required parameter '_double' when calling testEndpointParameters");
    }
    
    // verify the required parameter 'string' is set
    if (string == null) {
      throw new ApiException(400, "Missing the required parameter 'string' when calling testEndpointParameters");
    }
    
    // verify the required parameter '_byte' is set
    if (_byte == null) {
      throw new ApiException(400, "Missing the required parameter '_byte' when calling testEndpointParameters");
    }
    
    // create path and map variables
    String localVarPath = "/fake".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    if (integer != null)
      localVarFormParams.put("integer", integer);
if (int32 != null)
      localVarFormParams.put("int32", int32);
if (int64 != null)
      localVarFormParams.put("int64", int64);
if (number != null)
      localVarFormParams.put("number", number);
if (_float != null)
      localVarFormParams.put("float", _float);
if (_double != null)
      localVarFormParams.put("double", _double);
if (string != null)
      localVarFormParams.put("string", string);
if (_byte != null)
      localVarFormParams.put("byte", _byte);
if (binary != null)
      localVarFormParams.put("binary", binary);
if (date != null)
      localVarFormParams.put("date", date);
if (dateTime != null)
      localVarFormParams.put("dateTime", dateTime);
if (password != null)
      localVarFormParams.put("password", password);

    final String[] localVarAccepts = {
      "application/xml", "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
}
