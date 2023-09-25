package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import java.math.BigDecimal;
import org.openapitools.client.model.ChildWithNullable;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.EnumClass;
import org.openapitools.client.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.openapitools.client.model.HealthCheckResult;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.OuterObjectWithEnumProperty;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.TestInlineFreeformAdditionalPropertiesRequest;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
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
   * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
   * @return a {@code FakeBigDecimalMap200Response}
   * @throws ApiException if fails to make API call
   */
  public FakeBigDecimalMap200Response fakeBigDecimalMap() throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/fake/BigDecimalMap".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<FakeBigDecimalMap200Response> localVarReturnType = new GenericType<FakeBigDecimalMap200Response>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Health check endpoint
   * 
   * @return a {@code HealthCheckResult}
   * @throws ApiException if fails to make API call
   */
  public HealthCheckResult fakeHealthGet() throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/fake/health".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<HealthCheckResult> localVarReturnType = new GenericType<HealthCheckResult>() {};
    return apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * test http signature authentication
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @param query1 query parameter (optional)
   * @param header1 header parameter (optional)
   * @throws ApiException if fails to make API call
   */
  public void fakeHttpSignatureTest(Pet pet, String query1, String header1) throws ApiException {
    Object localVarPostBody = pet;
    
    // verify the required parameter 'pet' is set
    if (pet == null) {
      throw new ApiException(400, "Missing the required parameter 'pet' when calling fakeHttpSignatureTest");
    }
    
    // create path and map variables
    String localVarPath = "/fake/http-signature-test".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_1", query1));

    if (header1 != null)
      localVarHeaderParams.put("header_1", apiClient.parameterToString(header1));

    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json", "application/xml"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "http_signature_test" };


    apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return a {@code Boolean}
   * @throws ApiException if fails to make API call
   */
  public Boolean fakeOuterBooleanSerialize(Boolean body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/boolean".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<Boolean> localVarReturnType = new GenericType<Boolean>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of object with outer number type
   * @param outerComposite Input composite as post body (optional)
   * @return a {@code OuterComposite}
   * @throws ApiException if fails to make API call
   */
  public OuterComposite fakeOuterCompositeSerialize(OuterComposite outerComposite) throws ApiException {
    Object localVarPostBody = outerComposite;
    
    // create path and map variables
    String localVarPath = "/fake/outer/composite".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<OuterComposite> localVarReturnType = new GenericType<OuterComposite>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return a {@code BigDecimal}
   * @throws ApiException if fails to make API call
   */
  public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/number".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<BigDecimal> localVarReturnType = new GenericType<BigDecimal>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return a {@code String}
   * @throws ApiException if fails to make API call
   */
  public String fakeOuterStringSerialize(String body) throws ApiException {
    Object localVarPostBody = body;
    
    // create path and map variables
    String localVarPath = "/fake/outer/string".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * Test serialization of enum (int) properties with examples
   * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
   * @return a {@code OuterObjectWithEnumProperty}
   * @throws ApiException if fails to make API call
   */
  public OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws ApiException {
    Object localVarPostBody = outerObjectWithEnumProperty;
    
    // verify the required parameter 'outerObjectWithEnumProperty' is set
    if (outerObjectWithEnumProperty == null) {
      throw new ApiException(400, "Missing the required parameter 'outerObjectWithEnumProperty' when calling fakePropertyEnumIntegerSerialize");
    }
    
    // create path and map variables
    String localVarPath = "/fake/property/enum-int".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      "*/*"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    GenericType<OuterObjectWithEnumProperty> localVarReturnType = new GenericType<OuterObjectWithEnumProperty>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * 
   * For this test, the body has to be a binary file.
   * @param body image to upload (required)
   * @throws ApiException if fails to make API call
   */
  public void testBodyWithBinary(File body) throws ApiException {
    Object localVarPostBody = body;
    
    // verify the required parameter 'body' is set
    if (body == null) {
      throw new ApiException(400, "Missing the required parameter 'body' when calling testBodyWithBinary");
    }
    
    // create path and map variables
    String localVarPath = "/fake/body-with-binary".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "image/png"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * 
   * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
   * @param fileSchemaTestClass  (required)
   * @throws ApiException if fails to make API call
   */
  public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) throws ApiException {
    Object localVarPostBody = fileSchemaTestClass;
    
    // verify the required parameter 'fileSchemaTestClass' is set
    if (fileSchemaTestClass == null) {
      throw new ApiException(400, "Missing the required parameter 'fileSchemaTestClass' when calling testBodyWithFileSchema");
    }
    
    // create path and map variables
    String localVarPath = "/fake/body-with-file-schema".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * 
   * 
   * @param query  (required)
   * @param user  (required)
   * @throws ApiException if fails to make API call
   */
  public void testBodyWithQueryParams(String query, User user) throws ApiException {
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
    String localVarPath = "/fake/body-with-query-params".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
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


    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param client client model (required)
   * @return a {@code Client}
   * @throws ApiException if fails to make API call
   */
  public Client testClientModel(Client client) throws ApiException {
    Object localVarPostBody = client;
    
    // verify the required parameter 'client' is set
    if (client == null) {
      throw new ApiException(400, "Missing the required parameter 'client' when calling testClientModel");
    }
    
    // create path and map variables
    String localVarPath = "/fake".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
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
    return apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
      }
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param string None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param paramCallback None (optional)
   * @throws ApiException if fails to make API call
   */
  public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws ApiException {
    Object localVarPostBody = null;
    
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
    String localVarPath = "/fake".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
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


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
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
   * @param enumQueryModelArray  (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @throws ApiException if fails to make API call
   */
  public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) throws ApiException {
    Object localVarPostBody = null;
    
    // create path and map variables
    String localVarPath = "/fake".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("multi", "enum_query_string_array", enumQueryStringArray));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string", enumQueryString));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_integer", enumQueryInteger));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_double", enumQueryDouble));
    localVarQueryParams.addAll(apiClient.parameterToPairs("multi", "enum_query_model_array", enumQueryModelArray));

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


    apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * Fake endpoint to test group parameters (optional)
   * Fake endpoint to test group parameters (optional)
   * @param requiredStringGroup Required String in group parameters (required)
   * @param requiredBooleanGroup Required Boolean in group parameters (required)
   * @param requiredInt64Group Required Integer in group parameters (required)
   * @param stringGroup String in group parameters (optional)
   * @param booleanGroup Boolean in group parameters (optional)
   * @param int64Group Integer in group parameters (optional)
   * @throws ApiException if fails to make API call
   */
  public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'requiredStringGroup' is set
    if (requiredStringGroup == null) {
      throw new ApiException(400, "Missing the required parameter 'requiredStringGroup' when calling testGroupParameters");
    }
    
    // verify the required parameter 'requiredBooleanGroup' is set
    if (requiredBooleanGroup == null) {
      throw new ApiException(400, "Missing the required parameter 'requiredBooleanGroup' when calling testGroupParameters");
    }
    
    // verify the required parameter 'requiredInt64Group' is set
    if (requiredInt64Group == null) {
      throw new ApiException(400, "Missing the required parameter 'requiredInt64Group' when calling testGroupParameters");
    }
    
    // create path and map variables
    String localVarPath = "/fake".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("", "required_string_group", requiredStringGroup));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "required_int64_group", requiredInt64Group));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "string_group", stringGroup));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "int64_group", int64Group));

    if (requiredBooleanGroup != null)
      localVarHeaderParams.put("required_boolean_group", apiClient.parameterToString(requiredBooleanGroup));
if (booleanGroup != null)
      localVarHeaderParams.put("boolean_group", apiClient.parameterToString(booleanGroup));

    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] { "bearer_test" };


    apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * test inline additionalProperties
   * 
   * @param requestBody request body (required)
   * @throws ApiException if fails to make API call
   */
  public void testInlineAdditionalProperties(Map<String, String> requestBody) throws ApiException {
    Object localVarPostBody = requestBody;
    
    // verify the required parameter 'requestBody' is set
    if (requestBody == null) {
      throw new ApiException(400, "Missing the required parameter 'requestBody' when calling testInlineAdditionalProperties");
    }
    
    // create path and map variables
    String localVarPath = "/fake/inline-additionalProperties".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * test inline free-form additionalProperties
   * 
   * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
   * @throws ApiException if fails to make API call
   */
  public void testInlineFreeformAdditionalProperties(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws ApiException {
    Object localVarPostBody = testInlineFreeformAdditionalPropertiesRequest;
    
    // verify the required parameter 'testInlineFreeformAdditionalPropertiesRequest' is set
    if (testInlineFreeformAdditionalPropertiesRequest == null) {
      throw new ApiException(400, "Missing the required parameter 'testInlineFreeformAdditionalPropertiesRequest' when calling testInlineFreeformAdditionalProperties");
    }
    
    // create path and map variables
    String localVarPath = "/fake/inline-freeform-additionalProperties".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @throws ApiException if fails to make API call
   */
  public void testJsonFormData(String param, String param2) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'param' is set
    if (param == null) {
      throw new ApiException(400, "Missing the required parameter 'param' when calling testJsonFormData");
    }
    
    // verify the required parameter 'param2' is set
    if (param2 == null) {
      throw new ApiException(400, "Missing the required parameter 'param2' when calling testJsonFormData");
    }
    
    // create path and map variables
    String localVarPath = "/fake/jsonFormData".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
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


    apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * test nullable parent property
   * 
   * @param childWithNullable request body (required)
   * @throws ApiException if fails to make API call
   */
  public void testNullable(ChildWithNullable childWithNullable) throws ApiException {
    Object localVarPostBody = childWithNullable;
    
    // verify the required parameter 'childWithNullable' is set
    if (childWithNullable == null) {
      throw new ApiException(400, "Missing the required parameter 'childWithNullable' when calling testNullable");
    }
    
    // create path and map variables
    String localVarPath = "/fake/nullable".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();


    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
  /**
   * 
   * To test the collection format in query parameters
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   * @param allowEmpty  (required)
   * @param language  (optional)
   * @throws ApiException if fails to make API call
   */
  public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'pipe' is set
    if (pipe == null) {
      throw new ApiException(400, "Missing the required parameter 'pipe' when calling testQueryParameterCollectionFormat");
    }
    
    // verify the required parameter 'ioutil' is set
    if (ioutil == null) {
      throw new ApiException(400, "Missing the required parameter 'ioutil' when calling testQueryParameterCollectionFormat");
    }
    
    // verify the required parameter 'http' is set
    if (http == null) {
      throw new ApiException(400, "Missing the required parameter 'http' when calling testQueryParameterCollectionFormat");
    }
    
    // verify the required parameter 'url' is set
    if (url == null) {
      throw new ApiException(400, "Missing the required parameter 'url' when calling testQueryParameterCollectionFormat");
    }
    
    // verify the required parameter 'context' is set
    if (context == null) {
      throw new ApiException(400, "Missing the required parameter 'context' when calling testQueryParameterCollectionFormat");
    }
    
    // verify the required parameter 'allowEmpty' is set
    if (allowEmpty == null) {
      throw new ApiException(400, "Missing the required parameter 'allowEmpty' when calling testQueryParameterCollectionFormat");
    }
    
    // create path and map variables
    String localVarPath = "/fake/test-query-parameters".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, String> localVarCookieParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    localVarQueryParams.addAll(apiClient.parameterToPairs("pipes", "pipe", pipe));
    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "ioutil", ioutil));
    localVarQueryParams.addAll(apiClient.parameterToPairs("ssv", "http", http));
    localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "url", url));
    localVarQueryParams.addAll(apiClient.parameterToPairs("multi", "context", context));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "language", language));
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "allowEmpty", allowEmpty));

    
    
    
    final String[] localVarAccepts = {
      
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };


    apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
  }
}
