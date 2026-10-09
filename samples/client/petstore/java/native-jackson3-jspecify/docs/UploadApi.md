# UploadApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**uploadFilesPost**](UploadApi.md#uploadFilesPost) | **POST** /uploadFiles |  |
| [**uploadFilesPostWithHttpInfo**](UploadApi.md#uploadFilesPostWithHttpInfo) | **POST** /uploadFiles |  |
| [**uploadPost**](UploadApi.md#uploadPost) | **POST** /upload |  |
| [**uploadPostWithHttpInfo**](UploadApi.md#uploadPostWithHttpInfo) | **POST** /upload |  |



## uploadFilesPost

> void uploadFilesPost(_file)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.UploadApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        UploadApi apiInstance = new UploadApi(defaultClient);
        List<File> _file = Arrays.asList(); // List<File> | 
        try {
            apiInstance.uploadFilesPost(_file);
        } catch (ApiException e) {
            System.err.println("Exception when calling UploadApi#uploadFilesPost");
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
| **_file** | **List&lt;File&gt;**|  | [optional] |

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

## uploadFilesPostWithHttpInfo

> ApiResponse<Void> uploadFilesPostWithHttpInfo(_file)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.UploadApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        UploadApi apiInstance = new UploadApi(defaultClient);
        List<File> _file = Arrays.asList(); // List<File> | 
        try {
            ApiResponse<Void> response = apiInstance.uploadFilesPostWithHttpInfo(_file);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
        } catch (ApiException e) {
            System.err.println("Exception when calling UploadApi#uploadFilesPost");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **_file** | **List&lt;File&gt;**|  | [optional] |

### Return type


ApiResponse<Void>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | ok |  -  |


## uploadPost

> void uploadPost(_file)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.UploadApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        UploadApi apiInstance = new UploadApi(defaultClient);
        File _file = new File("/path/to/file"); // File | 
        try {
            apiInstance.uploadPost(_file);
        } catch (ApiException e) {
            System.err.println("Exception when calling UploadApi#uploadPost");
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

## uploadPostWithHttpInfo

> ApiResponse<Void> uploadPostWithHttpInfo(_file)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.UploadApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        UploadApi apiInstance = new UploadApi(defaultClient);
        File _file = new File("/path/to/file"); // File | 
        try {
            ApiResponse<Void> response = apiInstance.uploadPostWithHttpInfo(_file);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
        } catch (ApiException e) {
            System.err.println("Exception when calling UploadApi#uploadPost");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
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


ApiResponse<Void>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | ok |  -  |

