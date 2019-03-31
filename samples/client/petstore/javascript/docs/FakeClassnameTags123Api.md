# OpenApiPetstore.FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case



## testClassname

> Client testClassname(body)

To test class name in snake case

To test class name in snake case

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure API key authorization: api_key_query
var api_key_query = defaultClient.authentications['api_key_query'];
api_key_query.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key_query.apiKeyPrefix = 'Token';

var apiInstance = new OpenApiPetstore.FakeClassnameTags123Api();
var body = new OpenApiPetstore.Client(); // Client | client model
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

[api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

