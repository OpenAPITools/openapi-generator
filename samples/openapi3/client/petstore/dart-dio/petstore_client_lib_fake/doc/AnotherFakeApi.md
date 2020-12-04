# openapi.api.AnotherFakeApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**123test@$%SpecialTags**](AnotherFakeApi.md#123test@$%SpecialTags) | **patch** /another-fake/dummy | To test special tags


# **123test@$%SpecialTags**
> Client 123test@$%SpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new AnotherFakeApi();
var client = new Client(); // Client | client model

try { 
    var result = api_instance.123test@$%SpecialTags(client);
    print(result);
} catch (e) {
    print("Exception when calling AnotherFakeApi->123test@$%SpecialTags: $e\n");
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

