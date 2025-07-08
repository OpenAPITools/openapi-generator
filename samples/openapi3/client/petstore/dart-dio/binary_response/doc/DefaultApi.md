# openapi.api.DefaultApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**binaryResponse**](DefaultApi.md#binaryresponse) | **GET** /limits | 


# **binaryResponse**
> Uint8List binaryResponse()



### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.binaryResponse();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->binaryResponse: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**Uint8List**](Uint8List.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/zip

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

