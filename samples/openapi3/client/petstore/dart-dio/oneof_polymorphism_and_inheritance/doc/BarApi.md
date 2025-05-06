# openapi.api.BarApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost:8080*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createBar**](BarApi.md#createbar) | **POST** /bar | Create a Bar


# **createBar**
> Bar createBar(barcreate)

Create a Bar

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getBarApi();
final BarCreate barcreate = ; // BarCreate | 

try {
    final response = api.createBar(barcreate);
    print(response);
} catch on DioException (e) {
    print('Exception when calling BarApi->createBar: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **barcreate** | [**BarCreate**](BarCreate.md)|  | 

### Return type

[**Bar**](Bar.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

