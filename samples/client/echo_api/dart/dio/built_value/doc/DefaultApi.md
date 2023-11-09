# openapi.api.DefaultApi

## Load the API package
```dart
import 'package:openapi/openapi.dart';
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fooBasicGet**](DefaultApi.md#foobasicget) | **GET** /foo-basic | 
[**list**](DefaultApi.md#list) | **GET** /oneof-primitive | 
[**oneofGet**](DefaultApi.md#oneofget) | **GET** /oneof | 
[**test**](DefaultApi.md#test) | **PUT** /variant1 | 
[**variant1Get**](DefaultApi.md#variant1get) | **GET** /variant1 | 
[**variant2Get**](DefaultApi.md#variant2get) | **GET** /variant2 | 


# **fooBasicGet**
> FooBasicGetDefaultResponse fooBasicGet()



### Example
```dart
import 'package:openapi/openapi.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.fooBasicGet();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->fooBasicGet: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FooBasicGetDefaultResponse**](FooBasicGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list**
> GigaOneOf list()



### Example
```dart
import 'package:openapi/openapi.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.list();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->list: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**GigaOneOf**](GigaOneOf.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **oneofGet**
> Fruit oneofGet()



### Example
```dart
import 'package:openapi/openapi.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.oneofGet();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->oneofGet: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**Fruit**](Fruit.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test**
> test(body)



### Example
```dart
import 'package:openapi/openapi.dart';

final api = Openapi().getDefaultApi();
final JsonObject body = ; // JsonObject | 

try {
    api.test(body);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->test: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **JsonObject**|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **variant1Get**
> FruitVariant1 variant1Get()



### Example
```dart
import 'package:openapi/openapi.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.variant1Get();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->variant1Get: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FruitVariant1**](FruitVariant1.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **variant2Get**
> FruitAllOfDisc variant2Get()



### Example
```dart
import 'package:openapi/openapi.dart';

final api = Openapi().getDefaultApi();

try {
    final response = api.variant2Get();
    print(response);
} catch on DioException (e) {
    print('Exception when calling DefaultApi->variant2Get: $e\n');
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FruitAllOfDisc**](FruitAllOfDisc.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

