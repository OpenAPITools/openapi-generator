# openapi.api.FakeClassnameTags123Api

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case


# **testClassname**
> ModelClient testClassname(modelClient)

To test class name in snake case

To test class name in snake case

### Example
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key_query
//defaultApiClient.getAuthentication<ApiKeyAuth>('api_key_query').apiKey = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//defaultApiClient.getAuthentication<ApiKeyAuth>('api_key_query').apiKeyPrefix = 'Bearer';

final api = Openapi().getFakeClassnameTags123Api();
final ModelClient modelClient = ; // ModelClient | client model

try {
    final response = api.testClassname(modelClient);
    print(response);
} catch on DioError (e) {
    print('Exception when calling FakeClassnameTags123Api->testClassname: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **modelClient** | [**ModelClient**](ModelClient.md)| client model | 

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

