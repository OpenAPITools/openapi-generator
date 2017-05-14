# SwaggerPetstore.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters


<a name="testClientModel"></a>
# **testClientModel**
> Client testClientModel(body)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.FakeApi();

var body = new SwaggerPetstore.Client(); // Client | client model

apiInstance.testClientModel(body).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

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
> testEndpointParameters(_number, _double, patternWithoutDelimiter, _byte, opts)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');
var defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure HTTP basic authorization: http_basic_test
var http_basic_test = defaultClient.authentications['http_basic_test'];
http_basic_test.username = 'YOUR USERNAME';
http_basic_test.password = 'YOUR PASSWORD';

var apiInstance = new SwaggerPetstore.FakeApi();

var _number = 3.4; // Number | None

var _double = 1.2; // Number | None

var patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None

var _byte = B; // Blob | None

var opts = { 
  'integer': 56, // Number | None
  'int32': 56, // Number | None
  'int64': 789, // Number | None
  '_float': 3.4, // Number | None
  '_string': "_string_example", // String | None
  'binary': B, // Blob | None
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
 **binary** | **Blob**| None | [optional] 
 **_date** | **Date**| None | [optional] 
 **dateTime** | **Date**| None | [optional] 
 **password** | **String**| None | [optional] 
 **callback** | **String**| None | [optional] 

### Return type

null (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

<a name="testEnumParameters"></a>
# **testEnumParameters**
> testEnumParameters(opts)

To test enum parameters

To test enum parameters

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.FakeApi();

var opts = { 
  'enumFormStringArray': ["enumFormStringArray_example"], // [String] | Form parameter enum test (string array)
  'enumFormString': "-efg", // String | Form parameter enum test (string)
  'enumHeaderStringArray': ["enumHeaderStringArray_example"], // [String] | Header parameter enum test (string array)
  'enumHeaderString': "-efg", // String | Header parameter enum test (string)
  'enumQueryStringArray': ["enumQueryStringArray_example"], // [String] | Query parameter enum test (string array)
  'enumQueryString': "-efg", // String | Query parameter enum test (string)
  'enumQueryInteger': 56, // Number | Query parameter enum test (double)
  'enumQueryDouble': 1.2 // Number | Query parameter enum test (double)
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
 **enumFormStringArray** | [**[String]**](String.md)| Form parameter enum test (string array) | [optional] 
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg]
 **enumHeaderStringArray** | [**[String]**](String.md)| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg]
 **enumQueryStringArray** | [**[String]**](String.md)| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **Number**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **Number**| Query parameter enum test (double) | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

