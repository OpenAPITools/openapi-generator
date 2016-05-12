# SwaggerPetstore.FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters


<a name="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(_number, _double, _string, _byte, opts)

Fake endpoint for testing various parameters

Fake endpoint for testing various parameters

### Example
```javascript
var SwaggerPetstore = require('swagger-petstore');

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

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

