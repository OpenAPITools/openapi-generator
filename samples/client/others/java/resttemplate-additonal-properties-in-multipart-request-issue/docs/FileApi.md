# FileApi

All URIs are relative to *http://localhost:8080*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createFile**](FileApi.md#createFile) | **POST** /api/v1/file |  |



## createFile

> createFile(documentBytes, documentType, properties, structured)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FileApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:8080");

        FileApi apiInstance = new FileApi(defaultClient);
        File documentBytes = new File("/path/to/file"); // File | 
        String documentType = "documentType_example"; // String | 
        Map<String, String> properties = new HashMap(); // Map<String, String> | 
        StructuredType structured = new StructuredType(); // StructuredType | 
        try {
            apiInstance.createFile(documentBytes, documentType, properties, structured);
        } catch (ApiException e) {
            System.err.println("Exception when calling FileApi#createFile");
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
| **documentBytes** | **File**|  | |
| **documentType** | **String**|  | |
| **properties** | [**Map&lt;String, String&gt;**](Map.md)|  | |
| **structured** | [**StructuredType**](StructuredType.md)|  | [optional] |

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
| **201** | File created successfully |  -  |

