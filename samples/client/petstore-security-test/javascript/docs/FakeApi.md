# SwaggerPetstoreEnd.FakeApi

All URIs are relative to *https://petstore.swagger.io  &#39; \&quot; &#x3D;end/v2  &#39; \&quot; &#x3D;end*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEnd**](FakeApi.md#testCodeInjectEnd) | **PUT** /fake | To test code injection  &#39; \&quot; &#x3D;end


<a name="testCodeInjectEnd"></a>
# **testCodeInjectEnd**
> testCodeInjectEnd(opts)

To test code injection  &#39; \&quot; &#x3D;end

### Example
```javascript
var SwaggerPetstoreEnd = require('swagger_petstore____end');

var apiInstance = new SwaggerPetstoreEnd.FakeApi();

var opts = { 
  'testCodeInjectEnd': "testCodeInjectEnd_example" // String | To test code injection  ' \" =end
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
 **testCodeInjectEnd** | **String**| To test code injection  &#39; \&quot; &#x3D;end | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */   =end
 - **Accept**: application/json, */   =end

