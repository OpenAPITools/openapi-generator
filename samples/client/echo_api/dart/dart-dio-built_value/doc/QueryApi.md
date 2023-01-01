# openapi.api.QueryApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testQueryIntegerBooleanString**](QueryApi.md#testqueryintegerbooleanstring) | **GET** /query/integer/boolean/string | Test query parameter(s)
[**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testquerystyleformexplodetruearraystring) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
[**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testquerystyleformexplodetrueobject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)


# **testQueryIntegerBooleanString**
> String testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getQueryApi();
final int integerQuery = 56; // int | 
final bool booleanQuery = true; // bool | 
final String stringQuery = stringQuery_example; // String | 

try {
    final response = api.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery);
    print(response);
} catch on DioError (e) {
    print('Exception when calling QueryApi->testQueryIntegerBooleanString: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerQuery** | **int**|  | [optional] 
 **booleanQuery** | **bool**|  | [optional] 
 **stringQuery** | **String**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeTrueArrayString**
> String testQueryStyleFormExplodeTrueArrayString(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getQueryApi();
final TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = ; // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 

try {
    final response = api.testQueryStyleFormExplodeTrueArrayString(queryObject);
    print(response);
} catch on DioError (e) {
    print('Exception when calling QueryApi->testQueryStyleFormExplodeTrueArrayString: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeTrueObject**
> String testQueryStyleFormExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```dart
import 'package:openapi/api.dart';

final api = Openapi().getQueryApi();
final Pet queryObject = ; // Pet | 

try {
    final response = api.testQueryStyleFormExplodeTrueObject(queryObject);
    print(response);
} catch on DioError (e) {
    print('Exception when calling QueryApi->testQueryStyleFormExplodeTrueObject: $e\n');
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**Pet**](.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

