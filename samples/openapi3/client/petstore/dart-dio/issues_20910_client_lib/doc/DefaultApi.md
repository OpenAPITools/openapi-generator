# openapi.api.DefaultApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testGet**](DefaultApi.md#testget) | **GET** /test | 
[**testPost**](DefaultApi.md#testpost) | **POST** /test | 


# **testGet**
> testGet(orderBy)



### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getDefaultApi();
final OrderBy orderBy = ; // OrderBy | 

try {
    api.testGet(orderBy);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->testGet: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderBy** | [**OrderBy**](.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testPost**
> testPost(testPostRequest)



### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getDefaultApi();
final TestPostRequest testPostRequest = ; // TestPostRequest | 

try {
    api.testPost(testPostRequest);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->testPost: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **testPostRequest** | [**TestPostRequest**](TestPostRequest.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

