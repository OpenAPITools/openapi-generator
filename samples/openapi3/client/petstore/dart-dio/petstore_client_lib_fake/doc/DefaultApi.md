# openapi.api.DefaultApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fooGet**](DefaultApi.md#fooget) | **GET** /foo | 


# **fooGet**
> FooGetDefaultResponse fooGet()



### Example
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: global_api_key_cookie
//defaultApiClient.getAuthentication<ApiKeyAuth>('global_api_key_cookie').apiKey = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//defaultApiClient.getAuthentication<ApiKeyAuth>('global_api_key_cookie').apiKeyPrefix = 'Bearer';
// TODO Configure API key authorization: global_api_key_header
//defaultApiClient.getAuthentication<ApiKeyAuth>('global_api_key_header').apiKey = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//defaultApiClient.getAuthentication<ApiKeyAuth>('global_api_key_header').apiKeyPrefix = 'Bearer';

final api = Openapi().getDefaultApi();

try {
    final response = api.fooGet();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->fooGet: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

[global_api_key_cookie](../README.md#global_api_key_cookie), [global_api_key_header](../README.md#global_api_key_header)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

