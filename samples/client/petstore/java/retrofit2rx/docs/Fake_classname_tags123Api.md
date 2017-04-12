# Fake_classname_tags123Api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](Fake_classname_tags123Api.md#testClassname) | **PATCH** fake_classname_test | To test class name in snake case


<a name="testClassname"></a>
# **testClassname**
> Client testClassname(body)

To test class name in snake case

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.Fake_classname_tags123Api;


Fake_classname_tags123Api apiInstance = new Fake_classname_tags123Api();
Client body = new Client(); // Client | client model
try {
    Client result = apiInstance.testClassname(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling Fake_classname_tags123Api#testClassname");
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

