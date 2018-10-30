# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


<a name="call123testSpecialTags"></a>
# **call123testSpecialTags**
> Client call123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.AnotherFakeApi;


AnotherFakeApi apiInstance = new AnotherFakeApi();
Client client = new Client(); // Client | client model
try {
    Client result = apiInstance.call123testSpecialTags(client);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AnotherFakeApi#call123testSpecialTags");
    e.printStackTrace();
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

