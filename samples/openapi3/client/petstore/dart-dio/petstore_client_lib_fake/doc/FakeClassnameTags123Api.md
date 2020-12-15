# openapi.api.FakeClassnameTags123Api

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **patch** /fake_classname_test | To test class name in snake case


# **testClassname**
> Client testClassname(client)

To test class name in snake case

To test class name in snake case

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key_query
//defaultApiClient.getAuthentication<ApiKeyAuth>('api_key_query').apiKey = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//defaultApiClient.getAuthentication<ApiKeyAuth>('api_key_query').apiKeyPrefix = 'Bearer';

var api_instance = new FakeClassnameTags123Api();
var client = new Client(); // Client | client model

try { 
    var result = api_instance.testClassname(client);
    print(result);
} catch (e) {
    print('Exception when calling FakeClassnameTags123Api->testClassname: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

