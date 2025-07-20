# openapi.api.DefaultApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test**](DefaultApi.md#test) | **GET** /test | test


# **test**
> ObjectThree test()

test

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.test();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->test: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**ObjectThree**](ObjectThree.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

