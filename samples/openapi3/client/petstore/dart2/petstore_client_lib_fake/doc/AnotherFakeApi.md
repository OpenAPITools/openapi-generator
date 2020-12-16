# openapi.api.AnotherFakeApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


# **call123testSpecialTags**
> Client call123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example 
```dart
import 'package:openapi/api.dart';

final api_instance = AnotherFakeApi();
final client = Client(); // Client | client model

try { 
    final result = api_instance.call123testSpecialTags(client);
    print(result);
} catch (e) {
    print('Exception when calling AnotherFakeApi->call123testSpecialTags: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

