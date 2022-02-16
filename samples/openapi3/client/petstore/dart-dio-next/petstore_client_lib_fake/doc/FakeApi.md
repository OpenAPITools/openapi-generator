# openapi.api.FakeApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet**](FakeApi.md#fakehealthget) | **GET** /fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApi.md#fakehttpsignaturetest) | **GET** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string | 
[**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakepropertyenumintegerserialize) | **POST** /fake/property/enum-int | 
[**testBodyWithBinary**](FakeApi.md#testbodywithbinary) | **PUT** /fake/body-with-binary | 
[**testBodyWithFileSchema**](FakeApi.md#testbodywithfileschema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testqueryparametercollectionformat) | **PUT** /fake/test-query-parameters | 


# **fakeHealthGet**
> HealthCheckResult fakeHealthGet()

Health check endpoint

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getFakeApi();

try {
    final response = api.fakeHealthGet();
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakeHealthGet: $e\n');
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

final api = Openapi().getFakeApi();
final Pet pet = ; // Pet | Pet object that needs to be added to the store
final String query1 = query1_example; // String | query parameter
final String header1 = header1_example; // String | header parameter

try {
    api.fakeHttpSignatureTest(pet, query1, header1);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakeHttpSignatureTest: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 
 **query1** | **String**| query parameter | [optional] 
 **header1** | **String**| header parameter | [optional] 

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

final api = Openapi().getFakeApi();
final bool body = true; // bool | Input boolean as post body

try {
    final response = api.fakeOuterBooleanSerialize(body);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakeOuterBooleanSerialize: $e\n');
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

final api = Openapi().getFakeApi();
final OuterComposite outerComposite = ; // OuterComposite | Input composite as post body

try {
    final response = api.fakeOuterCompositeSerialize(outerComposite);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakeOuterCompositeSerialize: $e\n');
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

final api = Openapi().getFakeApi();
final num body = 8.14; // num | Input number as post body

try {
    final response = api.fakeOuterNumberSerialize(body);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakeOuterNumberSerialize: $e\n');
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

final api = Openapi().getFakeApi();
final String body = body_example; // String | Input string as post body

try {
    final response = api.fakeOuterStringSerialize(body);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakeOuterStringSerialize: $e\n');
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

# **fakePropertyEnumIntegerSerialize**
> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)



Test serialization of enum (int) properties with examples

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getFakeApi();
final OuterObjectWithEnumProperty outerObjectWithEnumProperty = ; // OuterObjectWithEnumProperty | Input enum (int) as post body

try {
    final response = api.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->fakePropertyEnumIntegerSerialize: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerObjectWithEnumProperty** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)| Input enum (int) as post body | 

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithBinary**
> testBodyWithBinary(body)



For this test, the body has to be a binary file.

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getFakeApi();
final MultipartFile body = BINARY_DATA_HERE; // MultipartFile | image to upload

try {
    api.testBodyWithBinary(body);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testBodyWithBinary: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **MultipartFile**| image to upload | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: image/png
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request must reference a schema named `File`.

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getFakeApi();
final FileSchemaTestClass fileSchemaTestClass = ; // FileSchemaTestClass | 

try {
    api.testBodyWithFileSchema(fileSchemaTestClass);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testBodyWithFileSchema: $e\n');
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

final api = Openapi().getFakeApi();
final String query = query_example; // String | 
final User user = ; // User | 

try {
    api.testBodyWithQueryParams(query, user);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testBodyWithQueryParams: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  | 
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
> ModelClient testClientModel(modelClient)

To test \"client\" model

To test \"client\" model

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getFakeApi();
final ModelClient modelClient = ; // ModelClient | client model

try {
    final response = api.testClientModel(modelClient);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testClientModel: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **modelClient** | [**ModelClient**](ModelClient.md)| client model | 

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEndpointParameters**
> testEndpointParameters(number, double_, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```dart
import 'package:openapi/api.dart';
// TODO Configure HTTP basic authorization: http_basic_test
//defaultApiClient.getAuthentication<HttpBasicAuth>('http_basic_test').username = 'YOUR_USERNAME'
//defaultApiClient.getAuthentication<HttpBasicAuth>('http_basic_test').password = 'YOUR_PASSWORD';

final api = Openapi().getFakeApi();
final num number = 8.14; // num | None
final double double_ = 1.2; // double | None
final String patternWithoutDelimiter = patternWithoutDelimiter_example; // String | None
final String byte = BYTE_ARRAY_DATA_HERE; // String | None
final int integer = 56; // int | None
final int int32 = 56; // int | None
final int int64 = 789; // int | None
final double float = 3.4; // double | None
final String string = string_example; // String | None
final Uint8List binary = BINARY_DATA_HERE; // Uint8List | None
final Date date = 2013-10-20; // Date | None
final DateTime dateTime = 2013-10-20T19:20:30+01:00; // DateTime | None
final String password = password_example; // String | None
final String callback = callback_example; // String | None

try {
    api.testEndpointParameters(number, double_, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, callback);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testEndpointParameters: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **num**| None | 
 **double_** | **double**| None | 
 **patternWithoutDelimiter** | **String**| None | 
 **byte** | **String**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **double**| None | [optional] 
 **string** | **String**| None | [optional] 
 **binary** | **Uint8List**| None | [optional] 
 **date** | **Date**| None | [optional] 
 **dateTime** | **DateTime**| None | [optional] 
 **password** | **String**| None | [optional] 
 **callback** | **String**| None | [optional] 

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

final api = Openapi().getFakeApi();
final BuiltList<String> enumHeaderStringArray = ; // BuiltList<String> | Header parameter enum test (string array)
final String enumHeaderString = enumHeaderString_example; // String | Header parameter enum test (string)
final BuiltList<String> enumQueryStringArray = ; // BuiltList<String> | Query parameter enum test (string array)
final String enumQueryString = enumQueryString_example; // String | Query parameter enum test (string)
final int enumQueryInteger = 56; // int | Query parameter enum test (double)
final double enumQueryDouble = 1.2; // double | Query parameter enum test (double)
final BuiltList<String> enumFormStringArray = ; // BuiltList<String> | Form parameter enum test (string array)
final String enumFormString = enumFormString_example; // String | Form parameter enum test (string)

try {
    api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testEnumParameters: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**BuiltList&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to '-efg']
 **enumQueryStringArray** | [**BuiltList&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to '-efg']
 **enumQueryInteger** | **int**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **double**| Query parameter enum test (double) | [optional] 
 **enumFormStringArray** | [**BuiltList&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [default to '$']
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to '-efg']

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

final api = Openapi().getFakeApi();
final int requiredStringGroup = 56; // int | Required String in group parameters
final bool requiredBooleanGroup = true; // bool | Required Boolean in group parameters
final int requiredInt64Group = 789; // int | Required Integer in group parameters
final int stringGroup = 56; // int | String in group parameters
final bool booleanGroup = true; // bool | Boolean in group parameters
final int int64Group = 789; // int | Integer in group parameters

try {
    api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testGroupParameters: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **int**| Required String in group parameters | 
 **requiredBooleanGroup** | **bool**| Required Boolean in group parameters | 
 **requiredInt64Group** | **int**| Required Integer in group parameters | 
 **stringGroup** | **int**| String in group parameters | [optional] 
 **booleanGroup** | **bool**| Boolean in group parameters | [optional] 
 **int64Group** | **int**| Integer in group parameters | [optional] 

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

final api = Openapi().getFakeApi();
final BuiltMap<String, String> requestBody = ; // BuiltMap<String, String> | request body

try {
    api.testInlineAdditionalProperties(requestBody);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testInlineAdditionalProperties: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**BuiltMap&lt;String, String&gt;**](String.md)| request body | 

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

final api = Openapi().getFakeApi();
final String param = param_example; // String | field1
final String param2 = param2_example; // String | field2

try {
    api.testJsonFormData(param, param2);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testJsonFormData: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 | 
 **param2** | **String**| field2 | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language)



To test the collection format in query parameters

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getFakeApi();
final BuiltList<String> pipe = ; // BuiltList<String> | 
final BuiltList<String> ioutil = ; // BuiltList<String> | 
final BuiltList<String> http = ; // BuiltList<String> | 
final BuiltList<String> url = ; // BuiltList<String> | 
final BuiltList<String> context = ; // BuiltList<String> | 
final String allowEmpty = allowEmpty_example; // String | 
final BuiltMap<String, String> language = ; // BuiltMap<String, String> | 

try {
    api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language);
} catch on DioError (e) {
    print('Exception when calling FakeApi->testQueryParameterCollectionFormat: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**BuiltList&lt;String&gt;**](String.md)|  | 
 **ioutil** | [**BuiltList&lt;String&gt;**](String.md)|  | 
 **http** | [**BuiltList&lt;String&gt;**](String.md)|  | 
 **url** | [**BuiltList&lt;String&gt;**](String.md)|  | 
 **context** | [**BuiltList&lt;String&gt;**](String.md)|  | 
 **allowEmpty** | **String**|  | 
 **language** | [**BuiltMap&lt;String, String&gt;**](String.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

