# PingApi

All URIs are relative to *http://localhost:8082*

Method | HTTP request | Description
------------- | ------------- | -------------
[**postPing**](PingApi.md#postPing) | **POST** /ping | 


<a name="postPing"></a>
# **postPing**
> SomeObj postPing(someObj)



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.PingApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://localhost:8082");

    PingApi apiInstance = new PingApi(defaultClient);
    SomeObj someObj = new SomeObj(); // SomeObj | 
    try {
      SomeObj result = apiInstance.postPing(someObj);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling PingApi#postPing");
      System.err.println("Status code: " + e.getCode());
      System.err.println("Reason: " + e.getResponseBody());
      System.err.println("Response headers: " + e.getResponseHeaders());
      e.printStackTrace();
    }
  }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **someObj** | [**SomeObj**](SomeObj.md)|  | [optional]

### Return type

[**SomeObj**](SomeObj.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

