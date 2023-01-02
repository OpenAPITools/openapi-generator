# openapi.api.PathApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testsPathStringPathStringIntegerPathInteger**](PathApi.md#testspathstringpathstringintegerpathinteger) | **GET** /path/string/{path_string}/integer/{path_integer} | Test path parameter(s)


# **testsPathStringPathStringIntegerPathInteger**
> String testsPathStringPathStringIntegerPathInteger(pathString, pathInteger)

Test path parameter(s)

Test path parameter(s)

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getPathApi();
final String pathString = pathString_example; // String | 
final int pathInteger = 56; // int | 

try {
    final response = api.testsPathStringPathStringIntegerPathInteger(pathString, pathInteger);
    print(response);
} catch on DioError (e) {
    print('Exception when calling PathApi->testsPathStringPathStringIntegerPathInteger: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pathString** | **String**|  | 
 **pathInteger** | **int**|  | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

