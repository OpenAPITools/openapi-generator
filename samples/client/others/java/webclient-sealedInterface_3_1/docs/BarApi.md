# BarApi

All URIs are relative to *http://localhost:8080*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createBar**](BarApi.md#createBar) | **POST** /bar | Create a Bar |



## createBar

> Bar createBar(barCreate)

Create a Bar

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.BarApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:8080");

        BarApi apiInstance = new BarApi(defaultClient);
        BarCreate barCreate = new BarCreate(); // BarCreate | 
        try {
            Bar result = apiInstance.createBar(barCreate);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BarApi#createBar");
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
| **barCreate** | [**BarCreate**](BarCreate.md)|  | |

### Return type

[**Bar**](Bar.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Bar created |  -  |

