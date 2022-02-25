# openapi.api.AnotherFakeApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags


# **call123testSpecialTags**
> ModelClient call123testSpecialTags(modelClient)

To test special tags

To test special tags and operation ID starting with number

### Example
```dart
import 'package:openapi/api.dart';

var api_instance = new AnotherFakeApi();
var modelClient = new ModelClient(); // ModelClient | client model

try {
    var result = api_instance.call123testSpecialTags(modelClient);
    print(result);
} catch (e) {
    print('Exception when calling AnotherFakeApi->call123testSpecialTags: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **modelClient** | [**ModelClient**](ModelClient.md)| client model | 

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

