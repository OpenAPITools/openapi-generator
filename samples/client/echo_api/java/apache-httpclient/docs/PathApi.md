# PathApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testsPathStringPathStringIntegerPathInteger**](PathApi.md#testsPathStringPathStringIntegerPathInteger) | **GET** /path/string/{path_string}/integer/{path_integer} | Test path parameter(s) |



## testsPathStringPathStringIntegerPathInteger

> String testsPathStringPathStringIntegerPathInteger(pathString, pathInteger)

Test path parameter(s)

Test path parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.PathApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        PathApi apiInstance = new PathApi(defaultClient);
        String pathString = "pathString_example"; // String | 
        Integer pathInteger = 56; // Integer | 
        try {
            String result = apiInstance.testsPathStringPathStringIntegerPathInteger(pathString, pathInteger);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling PathApi#testsPathStringPathStringIntegerPathInteger");
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
| **pathString** | **String**|  | |
| **pathInteger** | **Integer**|  | |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

