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
> InlineResponseDefault fooGet()



### Example
```dart
import 'package:openapi/api.dart';

var api_instance = new DefaultApi();

try {
    var result = api_instance.fooGet();
    print(result);
} catch (e) {
    print('Exception when calling DefaultApi->fooGet: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**InlineResponseDefault**](InlineResponseDefault.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

