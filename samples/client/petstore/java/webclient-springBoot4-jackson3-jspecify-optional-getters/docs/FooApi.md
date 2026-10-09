# FooApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fooDtParamGet**](FooApi.md#fooDtParamGet) | **GET** /foo/{dtParam} |  |



## fooDtParamGet

> Foo fooDtParamGet(dtParam, dtQuery, dtCookie, color)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FooApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        FooApi apiInstance = new FooApi(defaultClient);
        java.time.Instant dtParam = new java.time.Instant(); // java.time.Instant | 
        java.time.Instant dtQuery = new java.time.Instant(); // java.time.Instant | 
        java.time.Instant dtCookie = new java.time.Instant(); // java.time.Instant | 
        String color = "red"; // String | 
        try {
            Foo result = apiInstance.fooDtParamGet(dtParam, dtQuery, dtCookie, color);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling FooApi#fooDtParamGet");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **dtParam** | **java.time.Instant**|  | [optional] |
| **dtQuery** | **java.time.Instant**|  | [optional] |
| **dtCookie** | **java.time.Instant**|  | [optional] |
| **color** | **String**|  | [optional] [default to red] |

### Return type

[**Foo**](Foo.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

