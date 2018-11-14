# OpenApiPetstore.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data


<a name="fakeOuterBooleanSerialize"></a>
# **fakeOuterBooleanSerialize**
> Boolean fakeOuterBooleanSerialize(opts)



Test serialization of outer boolean types

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': true // Boolean | Input boolean as post body
};
apiInstance.fakeOuterBooleanSerialize(opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

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
> OuterComposite fakeOuterCompositeSerialize(opts)



Test serialization of object with outer number type

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'outerComposite': new OpenApiPetstore.OuterComposite() // OuterComposite | Input composite as post body
};
apiInstance.fakeOuterCompositeSerialize(opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

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
> Number fakeOuterNumberSerialize(opts)



Test serialization of outer number types

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': 3.4 // Number | Input number as post body
};
apiInstance.fakeOuterNumberSerialize(opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Number**| Input number as post body | [optional] 

### Return type

**Number**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

<a name="fakeOuterStringSerialize"></a>
# **fakeOuterStringSerialize**
> String fakeOuterStringSerialize(opts)



Test serialization of outer string types

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': "body_example" // String | Input string as post body
};
apiInstance.fakeOuterStringSerialize(opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

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
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var fileSchemaTestClass = new OpenApiPetstore.FileSchemaTestClass(); // FileSchemaTestClass | 
apiInstance.testBodyWithFileSchema(fileSchemaTestClass).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

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
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var query = "query_example"; // String | 
var user = new OpenApiPetstore.User(); // User | 
apiInstance.testBodyWithQueryParams(query, user).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

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
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var client = new OpenApiPetstore.Client(); // Client | client model
apiInstance.testClientModel(client).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

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
> testEndpointParameters(_number, _double, patternWithoutDelimiter, _byte, opts)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;

// Configure HTTP basic authorization: http_basic_test
var http_basic_test = defaultClient.authentications['http_basic_test'];
http_basic_test.username = 'YOUR USERNAME';
http_basic_test.password = 'YOUR PASSWORD';

var apiInstance = new OpenApiPetstore.FakeApi();
var _number = 3.4; // Number | None
var _double = 3.4; // Number | None
var patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None
var _byte = null; // Blob | None
var opts = {
  'integer': 56, // Number | None
  'int32': 56, // Number | None
  'int64': 789, // Number | None
  '_float': 3.4, // Number | None
  '_string': "_string_example", // String | None
  'binary': "/path/to/file", // File | None
  '_date': new Date("2013-10-20"), // Date | None
  'dateTime': new Date("2013-10-20T19:20:30+01:00"), // Date | None
  'password': "password_example", // String | None
  'callback': "callback_example" // String | None
};
apiInstance.testEndpointParameters(_number, _double, patternWithoutDelimiter, _byte, opts).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_number** | **Number**| None | 
 **_double** | **Number**| None | 
 **patternWithoutDelimiter** | **String**| None | 
 **_byte** | **Blob**| None | 
 **integer** | **Number**| None | [optional] 
 **int32** | **Number**| None | [optional] 
 **int64** | **Number**| None | [optional] 
 **_float** | **Number**| None | [optional] 
 **_string** | **String**| None | [optional] 
 **binary** | **File**| None | [optional] 
 **_date** | **Date**| None | [optional] 
 **dateTime** | **Date**| None | [optional] 
 **password** | **String**| None | [optional] 
 **callback** | **String**| None | [optional] 

### Return type

null (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="testEnumParameters"></a>
# **testEnumParameters**
> testEnumParameters(opts)

To test enum parameters

To test enum parameters

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'enumHeaderStringArray': ["'$'"], // [String] | Header parameter enum test (string array)
  'enumHeaderString': "'-efg'", // String | Header parameter enum test (string)
  'enumQueryStringArray': ["'$'"], // [String] | Query parameter enum test (string array)
  'enumQueryString': "'-efg'", // String | Query parameter enum test (string)
  'enumQueryInteger': 56, // Number | Query parameter enum test (double)
  'enumQueryDouble': 3.4, // Number | Query parameter enum test (double)
  'enumFormStringArray': "'$'", // [String] | Form parameter enum test (string array)
  'enumFormString': "'-efg'" // String | Form parameter enum test (string)
};
apiInstance.testEnumParameters(opts).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**[String]**](String.md)| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enumQueryStringArray** | [**[String]**](String.md)| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enumQueryInteger** | **Number**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **Number**| Query parameter enum test (double) | [optional] 
 **enumFormStringArray** | [**[String]**](String.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

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
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var requestBody = {key: "null"}; // {String: String} | request body
apiInstance.testInlineAdditionalProperties(requestBody).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**{String: String}**](String.md)| request body | 

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
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var param = "param_example"; // String | field1
var param2 = "param2_example"; // String | field2
apiInstance.testJsonFormData(param, param2).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

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

