# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fooDtParamGet**](DefaultApi.md#fooDtParamGet) | **GET** /foo/{dtParam} |  |
| [**uploadPost**](DefaultApi.md#uploadPost) | **POST** /upload |  |



## fooDtParamGet

> Foo fooDtParamGet(dtParam, dtQuery, dtCookie)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        DefaultApi apiInstance = new DefaultApi(defaultClient);
        java.time.Instant dtParam = new java.time.Instant(); // java.time.Instant | 
        java.time.Instant dtQuery = new java.time.Instant(); // java.time.Instant | 
        java.time.Instant dtCookie = new java.time.Instant(); // java.time.Instant | 
        try {
            Foo result = apiInstance.fooDtParamGet(dtParam, dtQuery, dtCookie);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling DefaultApi#fooDtParamGet");
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


## uploadPost

> uploadPost(_file)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        DefaultApi apiInstance = new DefaultApi(defaultClient);
        File _file = new File("/path/to/file"); // File | 
        try {
            apiInstance.uploadPost(_file);
        } catch (ApiException e) {
            System.err.println("Exception when calling DefaultApi#uploadPost");
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
| **_file** | **File**|  | [optional] |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | ok |  -  |

