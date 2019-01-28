# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createXmlItem**](FakeApi.md#createXmlItem) | **POST** /fake/create_xml_item | creates an XmlItem
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data


<a name="createXmlItem"></a>
# **createXmlItem**
> createXmlItem(xmlItem)

creates an XmlItem

this route creates an XmlItem

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
XmlItem xmlItem = new XmlItem(); // XmlItem | XmlItem Body
try {
    apiInstance.createXmlItem(xmlItem);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#createXmlItem");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

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
> OuterComposite fakeOuterCompositeSerialize(body)



Test serialization of object with outer number type

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
OuterComposite body = new OuterComposite(); // OuterComposite | Input composite as post body
try {
    OuterComposite result = apiInstance.fakeOuterCompositeSerialize(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#fakeOuterCompositeSerialize");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional]

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
> testBodyWithFileSchema(body)



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
FileSchemaTestClass body = new FileSchemaTestClass(); // FileSchemaTestClass | 
try {
    apiInstance.testBodyWithFileSchema(body);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testBodyWithFileSchema");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a name="testBodyWithQueryParams"></a>
# **testBodyWithQueryParams**
> testBodyWithQueryParams(query, body)



### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
String query = "query_example"; // String | 
User body = new User(); // User | 
try {
    apiInstance.testBodyWithQueryParams(query, body);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testBodyWithQueryParams");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  |
 **body** | [**User**](User.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a name="testClientModel"></a>
# **testClientModel**
> Client testClientModel(body)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Client body = new Client(); // Client | client model
try {
    Client result = apiInstance.testClientModel(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testClientModel");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model |

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
String patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None
byte[] _byte = null; // byte[] | None
Integer integer = 56; // Integer | None
Integer int32 = 56; // Integer | None
Long int64 = 56L; // Long | None
Float _float = 3.4F; // Float | None
String string = "string_example"; // String | None
File binary = new File("/path/to/file"); // File | None
LocalDate date = new LocalDate(); // LocalDate | None
OffsetDateTime dateTime = new OffsetDateTime(); // OffsetDateTime | None
String password = "password_example"; // String | None
String paramCallback = "paramCallback_example"; // String | None
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
 **number** | **BigDecimal**| None |
 **_double** | **Double**| None |
 **patternWithoutDelimiter** | **String**| None |
 **_byte** | **byte[]**| None |
 **integer** | **Integer**| None | [optional]
 **int32** | **Integer**| None | [optional]
 **int64** | **Long**| None | [optional]
 **_float** | **Float**| None | [optional]
 **string** | **String**| None | [optional]
 **binary** | **File**| None | [optional]
 **date** | **LocalDate**| None | [optional]
 **dateTime** | **OffsetDateTime**| None | [optional]
 **password** | **String**| None | [optional]
 **paramCallback** | **String**| None | [optional]

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
List<String> enumHeaderStringArray = Arrays.asList("$"); // List<String> | Header parameter enum test (string array)
String enumHeaderString = "-efg"; // String | Header parameter enum test (string)
List<String> enumQueryStringArray = Arrays.asList("$"); // List<String> | Query parameter enum test (string array)
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
 **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [enum: >, $]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="testGroupParameters"></a>
# **testGroupParameters**
> testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group).stringGroup(stringGroup).booleanGroup(booleanGroup).int64Group(int64Group).execute();

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Integer requiredStringGroup = 56; // Integer | Required String in group parameters
Boolean requiredBooleanGroup = true; // Boolean | Required Boolean in group parameters
Long requiredInt64Group = 56L; // Long | Required Integer in group parameters
Integer stringGroup = 56; // Integer | String in group parameters
Boolean booleanGroup = true; // Boolean | Boolean in group parameters
Long int64Group = 56L; // Long | Integer in group parameters
try {
    apiInstance.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group)
            .stringGroup(stringGroup)
            .booleanGroup(booleanGroup)
            .int64Group(int64Group)
            .execute();
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testGroupParameters");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **Integer**| Required String in group parameters |
 **requiredBooleanGroup** | **Boolean**| Required Boolean in group parameters |
 **requiredInt64Group** | **Long**| Required Integer in group parameters |
 **stringGroup** | **Integer**| String in group parameters | [optional]
 **booleanGroup** | **Boolean**| Boolean in group parameters | [optional]
 **int64Group** | **Long**| Integer in group parameters | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="testInlineAdditionalProperties"></a>
# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(param)

test inline additionalProperties

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
Map<String, String> param = new HashMap(); // Map<String, String> | request body
try {
    apiInstance.testInlineAdditionalProperties(param);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testInlineAdditionalProperties");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**Map&lt;String, String&gt;**](String.md)| request body |

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
String param = "param_example"; // String | field1
String param2 = "param2_example"; // String | field2
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
 **param** | **String**| field1 |
 **param2** | **String**| field2 |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

