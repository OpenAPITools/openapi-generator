# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testSpecialTags**](AnotherFakeApi.md#testSpecialTags) | **PATCH** another-fake/dummy | To test special tags


<a name="testSpecialTags"></a>
# **testSpecialTags**
> Client testSpecialTags(body)

To test special tags

To test special tags

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.AnotherFakeApi;


AnotherFakeApi apiInstance = new AnotherFakeApi();
Client body = new Client(); // Client | client model
try {
    Client result = apiInstance.testSpecialTags(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnotherFakeApi#testSpecialTags");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

