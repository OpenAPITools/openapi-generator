# SwaggerPetstoreEndRnNR.FakeApi

All URIs are relative to *https://petstore.swagger.io *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r/v2 *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEndRnNR**](FakeApi.md#testCodeInjectEndRnNR) | **PUT** /fake | To test code injection *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r


<a name="testCodeInjectEndRnNR"></a>
# **testCodeInjectEndRnNR**
> testCodeInjectEndRnNR(opts)

To test code injection *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r

### Example
```javascript
var SwaggerPetstoreEndRnNR = require('swagger_petstore____end____rn_n_r');

var apiInstance = new SwaggerPetstoreEndRnNR.FakeApi();

var opts = { 
  'testCodeInjectEndRnNR': "testCodeInjectEndRnNR_example" // String | To test code injection *_/ ' \" =end -- \\r\\n \\n \\r
};

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testCodeInjectEndRnNR(opts, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **testCodeInjectEndRnNR** | **String**| To test code injection *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, *_/   =end --       
 - **Accept**: application/json, *_/   =end --       

