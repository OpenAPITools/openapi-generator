# SwaggerPetstore.Fake_classname_tags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](Fake_classname_tags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case


<a name="testClassname"></a>
# **testClassname**
> Client testClassname(body)

To test class name in snake case

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.Fake_classname_tags123Api();

var body = new SwaggerPetstore.Client(); // Client | client model


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.testClassname(body, callback);
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

