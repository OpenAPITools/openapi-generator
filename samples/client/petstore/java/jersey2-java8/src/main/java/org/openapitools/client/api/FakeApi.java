package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;

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
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   * @throws ApiException if fails to make API call
   */
  public Boolean fakeOuterBooleanSerialize(Boolean body) throws ApiException {
    return fakeOuterBooleanSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return ApiResponse&lt;Boolean&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Boolean> fakeOuterBooleanSerializeWithHttpInfo(Boolean body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/boolean";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<Boolean> localVarReturnType = new GenericType<Boolean>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of object with outer number type
   * @param outerComposite Input composite as post body (optional)
   * @return OuterComposite
   * @throws ApiException if fails to make API call
   */
  public OuterComposite fakeOuterCompositeSerialize(OuterComposite outerComposite) throws ApiException {
    return fakeOuterCompositeSerializeWithHttpInfo(outerComposite).getData();
      }

  /**
   * 
   * Test serialization of object with outer number type
   * @param outerComposite Input composite as post body (optional)
   * @return ApiResponse&lt;OuterComposite&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo(OuterComposite outerComposite) throws ApiException {
    Object localVarPostBody = outerComposite;
    
    // create path and map variables
    String localVarPath = "/fake/outer/composite";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<OuterComposite> localVarReturnType = new GenericType<OuterComposite>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   * @throws ApiException if fails to make API call
   */
  public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws ApiException {
    return fakeOuterNumberSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return ApiResponse&lt;BigDecimal&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<BigDecimal> fakeOuterNumberSerializeWithHttpInfo(BigDecimal body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/number";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<BigDecimal> localVarReturnType = new GenericType<BigDecimal>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return String
   * @throws ApiException if fails to make API call
   */
  public String fakeOuterStringSerialize(String body) throws ApiException {
    return fakeOuterStringSerializeWithHttpInfo(body).getData();
      }

  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return ApiResponse&lt;String&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<String> fakeOuterStringSerializeWithHttpInfo(String body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/string";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
   * @param fileSchemaTestClass  (required)
   * @throws ApiException if fails to make API call
   */
  public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) throws ApiException {

    testBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass);
  }

  /**
   * 
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
   * @param fileSchemaTestClass  (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testBodyWithFileSchemaWithHttpInfo(FileSchemaTestClass fileSchemaTestClass) throws ApiException {
    Object localVarPostBody = fileSchemaTestClass;
    
    // verify the required parameter 'fileSchemaTestClass' is set
    if (fileSchemaTestClass == null) {
      throw new ApiException(400, "Missing the required parameter 'fileSchemaTestClass' when calling testBodyWithFileSchema");
    }
    
    // create path and map variables
    String localVarPath = "/fake/body-with-file-schema";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * 
   * 
   * @param query  (required)
   * @param user  (required)
   * @throws ApiException if fails to make API call
   */
  public void testBodyWithQueryParams(String query, User user) throws ApiException {

    testBodyWithQueryParamsWithHttpInfo(query, user);
  }

  /**
   * 
   * 
   * @param query  (required)
   * @param user  (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testBodyWithQueryParamsWithHttpInfo(String query, User user) throws ApiException {
    Object localVarPostBody = user;
    
    // verify the required parameter 'query' is set
    if (query == null) {
      throw new ApiException(400, "Missing the required parameter 'query' when calling testBodyWithQueryParams");
    }
    
    // verify the required parameter 'user' is set
    if (user == null) {
      throw new ApiException(400, "Missing the required parameter 'user' when calling testBodyWithQueryParams");
    }
    
    // create path and map variables
    String localVarPath = "/fake/body-with-query-params";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query", query));

    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param client client model (required)
   * @return Client
   * @throws ApiException if fails to make API call
   */
  public Client testClientModel(Client client) throws ApiException {
    return testClientModelWithHttpInfo(client).getData();
      }

  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param client client model (required)
   * @return ApiResponse&lt;Client&gt;
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Client> testClientModelWithHttpInfo(Client client) throws ApiException {
    Object localVarPostBody = client;
    
    // verify the required parameter 'client' is set
    if (client == null) {
      throw new ApiException(400, "Missing the required parameter 'client' when calling testClientModel");
    }
    
    // create path and map variables
    String localVarPath = "/fake";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<Client> localVarReturnType = new GenericType<Client>() {};
    return apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional, default to null)
   * @param int32 None (optional, default to null)
   * @param int64 None (optional, default to null)
   * @param _float None (optional, default to null)
   * @param string None (optional, default to null)
   * @param binary None (optional, default to null)
   * @param date None (optional, default to null)
   * @param dateTime None (optional, default to null)
   * @param password None (optional, default to null)
   * @param paramCallback None (optional, default to null)
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws ApiException {

    testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
  }

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional, default to null)
   * @param int32 None (optional, default to null)
   * @param int64 None (optional, default to null)
   * @param _float None (optional, default to null)
   * @param string None (optional, default to null)
   * @param binary None (optional, default to null)
   * @param date None (optional, default to null)
   * @param dateTime None (optional, default to null)
   * @param password None (optional, default to null)
   * @param paramCallback None (optional, default to null)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws ApiException {
    Object localVarPostBody = new Object();
    
    // verify the required parameter 'number' is set
    if (number == null) {
      throw new ApiException(400, "Missing the required parameter 'number' when calling testEndpointParameters");
    }
    
    // verify the required parameter '_double' is set
    if (_double == null) {
      throw new ApiException(400, "Missing the required parameter '_double' when calling testEndpointParameters");
    }
    
    // verify the required parameter 'patternWithoutDelimiter' is set
    if (patternWithoutDelimiter == null) {
      throw new ApiException(400, "Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters");
    }
    
    // verify the required parameter '_byte' is set
    if (_byte == null) {
      throw new ApiException(400, "Missing the required parameter '_byte' when calling testEndpointParameters");
    }
    
    // create path and map variables
    String localVarPath = "/fake";

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
if (patternWithoutDelimiter != null)
      localVarFormParams.put("pattern_without_delimiter", patternWithoutDelimiter);
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
if (paramCallback != null)
      localVarFormParams.put("callback", paramCallback);

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "http_basic_test" };


    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @throws ApiException if fails to make API call
   */
  public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws ApiException {

    testEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
  }

  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testEnumParametersWithHttpInfo(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws ApiException {
    Object localVarPostBody = new Object();
    
    // create path and map variables
    String localVarPath = "/fake";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "enum_query_string_array", enumQueryStringArray));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string", enumQueryString));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_integer", enumQueryInteger));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_double", enumQueryDouble));

    if (enumHeaderStringArray != null)
      localVarHeaderParams.put("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
if (enumHeaderString != null)
      localVarHeaderParams.put("enum_header_string", apiClient.parameterToString(enumHeaderString));

    if (enumFormStringArray != null)
      localVarFormParams.put("enum_form_string_array", enumFormStringArray);
if (enumFormString != null)
      localVarFormParams.put("enum_form_string", enumFormString);

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * test inline additionalProperties
   * 
   * @param requestBody request body (required)
   * @throws ApiException if fails to make API call
   */
  public void testInlineAdditionalProperties(Map<String, String> requestBody) throws ApiException {

    testInlineAdditionalPropertiesWithHttpInfo(requestBody);
  }

  /**
   * test inline additionalProperties
   * 
   * @param requestBody request body (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testInlineAdditionalPropertiesWithHttpInfo(Map<String, String> requestBody) throws ApiException {
    Object localVarPostBody = requestBody;
    
    // verify the required parameter 'requestBody' is set
    if (requestBody == null) {
      throw new ApiException(400, "Missing the required parameter 'requestBody' when calling testInlineAdditionalProperties");
    }
    
    // create path and map variables
    String localVarPath = "/fake/inline-additionalProperties";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @throws ApiException if fails to make API call
   */
  public void testJsonFormData(String param, String param2) throws ApiException {

    testJsonFormDataWithHttpInfo(param, param2);
  }

  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @throws ApiException if fails to make API call
   */
  public ApiResponse<Void> testJsonFormDataWithHttpInfo(String param, String param2) throws ApiException {
    Object localVarPostBody = new Object();
    
    // verify the required parameter 'param' is set
    if (param == null) {
      throw new ApiException(400, "Missing the required parameter 'param' when calling testJsonFormData");
    }
    
    // verify the required parameter 'param2' is set
    if (param2 == null) {
      throw new ApiException(400, "Missing the required parameter 'param2' when calling testJsonFormData");
    }
    
    // create path and map variables
    String localVarPath = "/fake/jsonFormData";

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    if (param != null)
      localVarFormParams.put("param", param);
if (param2 != null)
      localVarFormParams.put("param2", param2);

    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/x-www-form-urlencoded"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
}
