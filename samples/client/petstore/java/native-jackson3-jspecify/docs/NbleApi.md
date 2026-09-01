# NbleApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**nbleGet**](NbleApi.md#nbleGet) | **GET** /nble |  |
| [**nbleGetWithHttpInfo**](NbleApi.md#nbleGetWithHttpInfo) | **GET** /nble |  |
| [**nblePost**](NbleApi.md#nblePost) | **POST** /nble |  |
| [**nblePostWithHttpInfo**](NbleApi.md#nblePostWithHttpInfo) | **POST** /nble |  |



## nbleGet

> void nbleGet(mandatory, optional)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.NbleApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        NbleApi apiInstance = new NbleApi(defaultClient);
        String mandatory = "mandatory_example"; // String | 
        String optional = "optional_example"; // String | 
        try {
            apiInstance.nbleGet(mandatory, optional);
        } catch (ApiException e) {
            System.err.println("Exception when calling NbleApi#nbleGet");
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
| **mandatory** | **String**|  | |
| **optional** | **String**|  | [optional] |

### Return type


null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | OK |  -  |

## nbleGetWithHttpInfo

> ApiResponse<Void> nbleGetWithHttpInfo(mandatory, optional)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.NbleApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        NbleApi apiInstance = new NbleApi(defaultClient);
        String mandatory = "mandatory_example"; // String | 
        String optional = "optional_example"; // String | 
        try {
            ApiResponse<Void> response = apiInstance.nbleGetWithHttpInfo(mandatory, optional);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
        } catch (ApiException e) {
            System.err.println("Exception when calling NbleApi#nbleGet");
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
| **mandatory** | **String**|  | |
| **optional** | **String**|  | [optional] |

### Return type


ApiResponse<Void>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | OK |  -  |


## nblePost

> RequiredAndNullable nblePost(requiredAndNullable)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.NbleApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        NbleApi apiInstance = new NbleApi(defaultClient);
        RequiredAndNullable requiredAndNullable = new RequiredAndNullable(); // RequiredAndNullable | bodyWithRequiredAndNullableAttributes
        try {
            RequiredAndNullable result = apiInstance.nblePost(requiredAndNullable);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling NbleApi#nblePost");
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
| **requiredAndNullable** | [**RequiredAndNullable**](RequiredAndNullable.md)| bodyWithRequiredAndNullableAttributes | |

### Return type

[**RequiredAndNullable**](RequiredAndNullable.md)


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

## nblePostWithHttpInfo

> ApiResponse<RequiredAndNullable> nblePostWithHttpInfo(requiredAndNullable)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.NbleApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        NbleApi apiInstance = new NbleApi(defaultClient);
        RequiredAndNullable requiredAndNullable = new RequiredAndNullable(); // RequiredAndNullable | bodyWithRequiredAndNullableAttributes
        try {
            ApiResponse<RequiredAndNullable> response = apiInstance.nblePostWithHttpInfo(requiredAndNullable);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling NbleApi#nblePost");
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
| **requiredAndNullable** | [**RequiredAndNullable**](RequiredAndNullable.md)| bodyWithRequiredAndNullableAttributes | |

### Return type

ApiResponse<[**RequiredAndNullable**](RequiredAndNullable.md)>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

