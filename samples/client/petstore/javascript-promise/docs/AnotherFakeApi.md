# OpenApiPetstore.AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


<a name="call123testSpecialTags"></a>
# **call123testSpecialTags**
> Client call123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example
```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.AnotherFakeApi();
var client = new OpenApiPetstore.Client(); // Client | client model
apiInstance.call123testSpecialTags(client).then(function(data) {
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

