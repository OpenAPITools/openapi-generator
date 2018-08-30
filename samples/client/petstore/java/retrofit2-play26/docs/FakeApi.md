# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** fake | To test enum parameters
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** fake/jsonFormData | test json serialization of form data


<a name="fakeOuterBooleanSerialize"></a>
# **fakeOuterBooleanSerialize**
> Boolean fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Boolean body = true; // Boolean | Input boolean as post body
try {
    Boolean result = apiInstance.fakeOuterBooleanSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterBooleanSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Boolean**| Input boolean as post body | [optional]

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

<a name="fakeOuterCompositeSerialize"></a>
# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize(outerComposite)



Test serialization of object with outer number type

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
OuterComposite outerComposite = new OuterComposite(); // OuterComposite | Input composite as post body
try {
    OuterComposite result = apiInstance.fakeOuterCompositeSerialize(outerComposite);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterCompositeSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

<a name="fakeOuterNumberSerialize"></a>
# **fakeOuterNumberSerialize**
> BigDecimal fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
BigDecimal body = new BigDecimal(); // BigDecimal | Input number as post body
try {
    BigDecimal result = apiInstance.fakeOuterNumberSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterNumberSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **BigDecimal**| Input number as post body | [optional]

### Return type

[**BigDecimal**](BigDecimal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

<a name="fakeOuterStringSerialize"></a>
# **fakeOuterStringSerialize**
> String fakeOuterStringSerialize(body)



Test serialization of outer string types

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
String body = "body_example"; // String | Input string as post body
try {
    String result = apiInstance.fakeOuterStringSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterStringSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String**| Input string as post body | [optional]

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

<a name="testBodyWithFileSchema"></a>
# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
FileSchemaTestClass fileSchemaTestClass = new FileSchemaTestClass(); // FileSchemaTestClass | 
try {
    apiInstance.testBodyWithFileSchema(fileSchemaTestClass);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testBodyWithFileSchema");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a name="testBodyWithQueryParams"></a>
# **testBodyWithQueryParams**
> testBodyWithQueryParams(query, user)



### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
String query = "query_example"; // String | 
User user = new User(); // User | 
try {
    apiInstance.testBodyWithQueryParams(query, user);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testBodyWithQueryParams");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  |
 **user** | [**User**](User.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a name="testClientModel"></a>
# **testClientModel**
> Client testClientModel(client)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Client client = new Client(); // Client | client model
try {
    Client result = apiInstance.testClientModel(client);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testClientModel");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```java
// Import classes:
//import org.openapitools.client.ApiClient;
//import org.openapitools.client.ApiException;
//import org.openapitools.client.Configuration;
//import org.openapitools.client.auth.*;
//import org.openapitools.client.api.FakeApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();

// Configure HTTP basic authorization: http_basic_test
HttpBasicAuth http_basic_test = (HttpBasicAuth) defaultClient.getAuthentication("http_basic_test");
http_basic_test.setUsername("YOUR USERNAME");
http_basic_test.setPassword("YOUR PASSWORD");

FakeApi apiInstance = new FakeApi();
BigDecimal number = new BigDecimal(); // BigDecimal | None
Double _double = 3.4D; // Double | None
String patternWithoutDelimiter = "null"; // String | None
byte[] _byte = null; // byte[] | None
Integer integer = null; // Integer | None
Integer int32 = null; // Integer | None
Long int64 = nullL; // Long | None
Float _float = nullF; // Float | None
String string = "null"; // String | None
File binary = new File("null"); // File | None
LocalDate date = new LocalDate(); // LocalDate | None
OffsetDateTime dateTime = new OffsetDateTime(); // OffsetDateTime | None
String password = "null"; // String | None
String paramCallback = "null"; // String | None
try {
    apiInstance.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testEndpointParameters");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **BigDecimal**| None | [default to null]
 **_double** | **Double**| None | [default to null]
 **patternWithoutDelimiter** | **String**| None | [default to null]
 **_byte** | **byte[]**| None | [default to null]
 **integer** | **Integer**| None | [optional] [default to null]
 **int32** | **Integer**| None | [optional] [default to null]
 **int64** | **Long**| None | [optional] [default to null]
 **_float** | **Float**| None | [optional] [default to null]
 **string** | **String**| None | [optional] [default to null]
 **binary** | **File**| None | [optional] [default to null]
 **date** | **LocalDate**| None | [optional] [default to null]
 **dateTime** | **OffsetDateTime**| None | [optional] [default to null]
 **password** | **String**| None | [optional] [default to null]
 **paramCallback** | **String**| None | [optional] [default to null]

### Return type

null (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="testEnumParameters"></a>
# **testEnumParameters**
> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
List<String> enumHeaderStringArray = Arrays.asList("enumHeaderStringArray_example"); // List<String> | Header parameter enum test (string array)
String enumHeaderString = "-efg"; // String | Header parameter enum test (string)
List<String> enumQueryStringArray = Arrays.asList("enumQueryStringArray_example"); // List<String> | Query parameter enum test (string array)
String enumQueryString = "-efg"; // String | Query parameter enum test (string)
Integer enumQueryInteger = 56; // Integer | Query parameter enum test (double)
Double enumQueryDouble = 3.4D; // Double | Query parameter enum test (double)
List<String> enumFormStringArray = "$"; // List<String> | Form parameter enum test (string array)
String enumFormString = "-efg"; // String | Form parameter enum test (string)
try {
    apiInstance.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testEnumParameters");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] [enum: >, $]
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] [enum: >, $]
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryInteger** | **Integer**| Query parameter enum test (double) | [optional] [enum: 1, -2]
 **enumQueryDouble** | **Double**| Query parameter enum test (double) | [optional] [enum: 1.1, -1.2]
 **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [default to $] [enum: >, $]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="testInlineAdditionalProperties"></a>
# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(requestBody)

test inline additionalProperties

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Map<String, String> requestBody = new HashMap(); // Map<String, String> | request body
try {
    apiInstance.testInlineAdditionalProperties(requestBody);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testInlineAdditionalProperties");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**Map&lt;String, String&gt;**](String.md)| request body |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a name="testJsonFormData"></a>
# **testJsonFormData**
> testJsonFormData(param, param2)

test json serialization of form data

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
String param = "null"; // String | field1
String param2 = "null"; // String | field2
try {
    apiInstance.testJsonFormData(param, param2);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testJsonFormData");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 | [default to null]
 **param2** | **String**| field2 | [default to null]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

