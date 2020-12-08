# openapi.api.FakeApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **get** /fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **get** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **post** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **post** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **post** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **post** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **put** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **put** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **patch** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **get** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **delete** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **post** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **get** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **put** /fake/test-query-paramters | 


# **fakeHealthGet**
> HealthCheckResult fakeHealthGet()

Health check endpoint

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();

try { 
    var result = api_instance.fakeHealthGet();
    print(result);
} catch (e) {
    print("Exception when calling FakeApi->fakeHealthGet: $e\n");
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeHttpSignatureTest**
> fakeHttpSignatureTest(pet, query1, header1)

test http signature authentication

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure HTTP basic authorization: http_signature_test
//defaultApiClient.getAuthentication<HttpBasicAuth>('http_signature_test').username = 'YOUR_USERNAME'
//defaultApiClient.getAuthentication<HttpBasicAuth>('http_signature_test').password = 'YOUR_PASSWORD';

var api_instance = new FakeApi();
var pet = new Pet(); // Pet | Pet object that needs to be added to the store
var query1 = query1_example; // String | query parameter
var header1 = header1_example; // String | header parameter

try { 
    api_instance.fakeHttpSignatureTest(pet, query1, header1);
} catch (e) {
    print("Exception when calling FakeApi->fakeHttpSignatureTest: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 
 **query1** | **String**| query parameter | [optional] [default to null]
 **header1** | **String**| header parameter | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterBooleanSerialize**
> bool fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var body = new bool(); // bool | Input boolean as post body

try { 
    var result = api_instance.fakeOuterBooleanSerialize(body);
    print(result);
} catch (e) {
    print("Exception when calling FakeApi->fakeOuterBooleanSerialize: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bool**| Input boolean as post body | [optional] 

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize(outerComposite)



Test serialization of object with outer number type

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var outerComposite = new OuterComposite(); // OuterComposite | Input composite as post body

try { 
    var result = api_instance.fakeOuterCompositeSerialize(outerComposite);
    print(result);
} catch (e) {
    print("Exception when calling FakeApi->fakeOuterCompositeSerialize: $e\n");
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

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterNumberSerialize**
> num fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var body = new num(); // num | Input number as post body

try { 
    var result = api_instance.fakeOuterNumberSerialize(body);
    print(result);
} catch (e) {
    print("Exception when calling FakeApi->fakeOuterNumberSerialize: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **num**| Input number as post body | [optional] 

### Return type

**num**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterStringSerialize**
> String fakeOuterStringSerialize(body)



Test serialization of outer string types

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var body = new String(); // String | Input string as post body

try { 
    var result = api_instance.fakeOuterStringSerialize(body);
    print(result);
} catch (e) {
    print("Exception when calling FakeApi->fakeOuterStringSerialize: $e\n");
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

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request much reference a schema named `File`.

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var fileSchemaTestClass = new FileSchemaTestClass(); // FileSchemaTestClass | 

try { 
    api_instance.testBodyWithFileSchema(fileSchemaTestClass);
} catch (e) {
    print("Exception when calling FakeApi->testBodyWithFileSchema: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithQueryParams**
> testBodyWithQueryParams(query, user)



### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var query = query_example; // String | 
var user = new User(); // User | 

try { 
    api_instance.testBodyWithQueryParams(query, user);
} catch (e) {
    print("Exception when calling FakeApi->testBodyWithQueryParams: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  | [default to null]
 **user** | [**User**](User.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testClientModel**
> Client testClientModel(client)

To test \"client\" model

To test \"client\" model

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var client = new Client(); // Client | client model

try { 
    var result = api_instance.testClientModel(client);
    print(result);
} catch (e) {
    print("Exception when calling FakeApi->testClientModel: $e\n");
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEndpointParameters**
> testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure HTTP basic authorization: http_basic_test
//defaultApiClient.getAuthentication<HttpBasicAuth>('http_basic_test').username = 'YOUR_USERNAME'
//defaultApiClient.getAuthentication<HttpBasicAuth>('http_basic_test').password = 'YOUR_PASSWORD';

var api_instance = new FakeApi();
var number = 8.14; // num | None
var double = 1.2; // double | None
var patternWithoutDelimiter = patternWithoutDelimiter_example; // String | None
var byte = BYTE_ARRAY_DATA_HERE; // String | None
var integer = 56; // int | None
var int32 = 56; // int | None
var int64 = 789; // int | None
var float = 3.4; // double | None
var string = string_example; // String | None
var binary = BINARY_DATA_HERE; // Uint8List | None
var date = 2013-10-20; // DateTime | None
var dateTime = 2013-10-20T19:20:30+01:00; // DateTime | None
var password = password_example; // String | None
var callback = callback_example; // String | None

try { 
    api_instance.testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback);
} catch (e) {
    print("Exception when calling FakeApi->testEndpointParameters: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **num**| None | [default to null]
 **double** | **double**| None | [default to null]
 **patternWithoutDelimiter** | **String**| None | [default to null]
 **byte** | **String**| None | [default to null]
 **integer** | **int**| None | [optional] [default to null]
 **int32** | **int**| None | [optional] [default to null]
 **int64** | **int**| None | [optional] [default to null]
 **float** | **double**| None | [optional] [default to null]
 **string** | **String**| None | [optional] [default to null]
 **binary** | **Uint8List**| None | [optional] [default to null]
 **date** | **DateTime**| None | [optional] [default to null]
 **dateTime** | **DateTime**| None | [optional] [default to null]
 **password** | **String**| None | [optional] [default to null]
 **callback** | **String**| None | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEnumParameters**
> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var enumHeaderStringArray = []; // List<String> | Header parameter enum test (string array)
var enumHeaderString = enumHeaderString_example; // String | Header parameter enum test (string)
var enumQueryStringArray = []; // List<String> | Query parameter enum test (string array)
var enumQueryString = enumQueryString_example; // String | Query parameter enum test (string)
var enumQueryInteger = 56; // int | Query parameter enum test (double)
var enumQueryDouble = 1.2; // double | Query parameter enum test (double)
var enumFormStringArray = [enumFormStringArray_example]; // List<String> | Form parameter enum test (string array)
var enumFormString = enumFormString_example; // String | Form parameter enum test (string)

try { 
    api_instance.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
} catch (e) {
    print("Exception when calling FakeApi->testEnumParameters: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] [default to const []]
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] [default to const []]
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enumQueryInteger** | **int**| Query parameter enum test (double) | [optional] [default to null]
 **enumQueryDouble** | **double**| Query parameter enum test (double) | [optional] [default to null]
 **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testGroupParameters**
> testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure HTTP basic authorization: bearer_test
//defaultApiClient.getAuthentication<HttpBasicAuth>('bearer_test').username = 'YOUR_USERNAME'
//defaultApiClient.getAuthentication<HttpBasicAuth>('bearer_test').password = 'YOUR_PASSWORD';

var api_instance = new FakeApi();
var requiredStringGroup = 56; // int | Required String in group parameters
var requiredBooleanGroup = true; // bool | Required Boolean in group parameters
var requiredInt64Group = 789; // int | Required Integer in group parameters
var stringGroup = 56; // int | String in group parameters
var booleanGroup = true; // bool | Boolean in group parameters
var int64Group = 789; // int | Integer in group parameters

try { 
    api_instance.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
} catch (e) {
    print("Exception when calling FakeApi->testGroupParameters: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **int**| Required String in group parameters | [default to null]
 **requiredBooleanGroup** | **bool**| Required Boolean in group parameters | [default to null]
 **requiredInt64Group** | **int**| Required Integer in group parameters | [default to null]
 **stringGroup** | **int**| String in group parameters | [optional] [default to null]
 **booleanGroup** | **bool**| Boolean in group parameters | [optional] [default to null]
 **int64Group** | **int**| Integer in group parameters | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(requestBody)

test inline additionalProperties

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var requestBody = new Map&lt;String, String&gt;(); // Map<String, String> | request body

try { 
    api_instance.testInlineAdditionalProperties(requestBody);
} catch (e) {
    print("Exception when calling FakeApi->testInlineAdditionalProperties: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**Map&lt;String, String&gt;**](String.md)| request body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testJsonFormData**
> testJsonFormData(param, param2)

test json serialization of form data

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var param = param_example; // String | field1
var param2 = param2_example; // String | field2

try { 
    api_instance.testJsonFormData(param, param2);
} catch (e) {
    print("Exception when calling FakeApi->testJsonFormData: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 | [default to null]
 **param2** | **String**| field2 | [default to null]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new FakeApi();
var pipe = []; // List<String> | 
var ioutil = []; // List<String> | 
var http = []; // List<String> | 
var url = []; // List<String> | 
var context = []; // List<String> | 

try { 
    api_instance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context);
} catch (e) {
    print("Exception when calling FakeApi->testQueryParameterCollectionFormat: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**List&lt;String&gt;**](String.md)|  | [default to const []]
 **ioutil** | [**List&lt;String&gt;**](String.md)|  | [default to const []]
 **http** | [**List&lt;String&gt;**](String.md)|  | [default to const []]
 **url** | [**List&lt;String&gt;**](String.md)|  | [default to const []]
 **context** | [**List&lt;String&gt;**](String.md)|  | [default to const []]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

