# openapi.api.BodyApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testEchoBodyPet**](BodyApi.md#testechobodypet) | **POST** /echo/body/Pet | Test body parameter(s)


# **testEchoBodyPet**
> Pet testEchoBodyPet(pet)

Test body parameter(s)

Test body parameter(s)

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getBodyApi();
final Pet pet = ; // Pet | Pet object that needs to be added to the store

try {
    final response = api.testEchoBodyPet(pet);
    print(response);
} catch on DioError (e) {
    print('Exception when calling BodyApi->testEchoBodyPet: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

