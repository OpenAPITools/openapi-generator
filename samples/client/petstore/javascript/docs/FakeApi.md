# SwaggerPetstore.FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEnd**](FakeApi.md#testCodeInjectEnd) | **PUT** /fake | To test code injection  &#x3D;end
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumQueryParameters**](FakeApi.md#testEnumQueryParameters) | **GET** /fake | To test enum query parameters


<a name="testCodeInjectEnd"></a>
# **testCodeInjectEnd**
> testCodeInjectEnd(opts)

To test code injection  &#x3D;end

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.FakeApi();

var opts = { 
  'testCodeInjectEnd': "testCodeInjectEnd_example" // String | To test code injection  =end
};

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testCodeInjectEnd(opts, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **testCodeInjectEnd** | **String**| To test code injection  &#x3D;end | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */ =end));(phpinfo(
 - **Accept**: application/json, */ end

<a name="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(_number, _double, _string, _byte, opts)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.FakeApi();

var _number = 3.4; // Number | None

var _double = 1.2; // Number | None

var _string = "_string_example"; // String | None

var _byte = "B"; // String | None

var opts = { 
  'integer': 56, // Integer | None
  'int32': 56, // Integer | None
  'int64': 789, // Integer | None
  '_float': 3.4, // Number | None
  'binary': "B", // String | None
  '_date': new Date("2013-10-20"), // Date | None
  'dateTime': new Date("2013-10-20T19:20:30+01:00"), // Date | None
  'password': "password_example" // String | None
};

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testEndpointParameters(_number, _double, _string, _byte, opts, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_number** | **Number**| None | 
 **_double** | **Number**| None | 
 **_string** | **String**| None | 
 **_byte** | **String**| None | 
 **integer** | **Integer**| None | [optional] 
 **int32** | **Integer**| None | [optional] 
 **int64** | **Integer**| None | [optional] 
 **_float** | **Number**| None | [optional] 
 **binary** | **String**| None | [optional] 
 **_date** | **Date**| None | [optional] 
 **dateTime** | **Date**| None | [optional] 
 **password** | **String**| None | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

<a name="testEnumQueryParameters"></a>
# **testEnumQueryParameters**
> testEnumQueryParameters(opts)

To test enum query parameters

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.FakeApi();

var opts = { 
  'enumQueryString': "-efg", // String | Query parameter enum test (string)
  'enumQueryInteger': 3.4, // Number | Query parameter enum test (double)
  'enumQueryDouble': 1.2 // Number | Query parameter enum test (double)
};

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testEnumQueryParameters(opts, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **Number**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **Number**| Query parameter enum test (double) | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

