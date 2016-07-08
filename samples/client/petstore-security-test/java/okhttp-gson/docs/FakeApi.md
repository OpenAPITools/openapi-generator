# FakeApi

All URIs are relative to *https://petstore.swagger.io  &#39; \&quot; &#x3D;end/v2  &#39; \&quot; &#x3D;end*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEnd**](FakeApi.md#testCodeInjectEnd) | **PUT** /fake | To test code injection  &#39; \&quot; &#x3D;end


<a name="testCodeInjectEnd"></a>
# **testCodeInjectEnd**
> testCodeInjectEnd(testCodeInjectEnd)

To test code injection  &#39; \&quot; &#x3D;end

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.FakeApi;


FakeApi apiInstance = new FakeApi();
String testCodeInjectEnd = "testCodeInjectEnd_example"; // String | To test code injection  ' \" =end
try {
    apiInstance.testCodeInjectEnd(testCodeInjectEnd);
} catch (ApiException e) {
    System.err.println("Exception when calling FakeApi#testCodeInjectEnd");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **testCodeInjectEnd** | **String**| To test code injection  &#39; \&quot; &#x3D;end | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */ '  =end
 - **Accept**: application/json, */ '  =end

