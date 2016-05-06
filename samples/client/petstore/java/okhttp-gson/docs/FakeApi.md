# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters


<a name="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password)

Fake endpoint for testing various parameters

Fake endpoint for testing various parameters

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
BigDecimal number = new BigDecimal(); // BigDecimal | None
Double _double = 3.4D; // Double | None
String string = "string_example"; // String | None
byte[] _byte = B; // byte[] | None
Integer integer = 56; // Integer | None
Integer int32 = 56; // Integer | None
Long int64 = 789L; // Long | None
Float _float = 3.4F; // Float | None
byte[] binary = B; // byte[] | None
Date date = new Date(); // Date | None
Date dateTime = new Date(); // Date | None
String password = "password_example"; // String | None
try {
    apiInstance.testEndpointParameters(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testEndpointParameters");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **BigDecimal**| None |
 **_double** | **Double**| None |
 **string** | **String**| None |
 **_byte** | **byte[]**| None |
 **integer** | **Integer**| None | [optional]
 **int32** | **Integer**| None | [optional]
 **int64** | **Long**| None | [optional]
 **_float** | **Float**| None | [optional]
 **binary** | **byte[]**| None | [optional]
 **date** | **Date**| None | [optional]
 **dateTime** | **Date**| None | [optional]
 **password** | **String**| None | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

