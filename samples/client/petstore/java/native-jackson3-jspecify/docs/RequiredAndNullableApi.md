# RequiredAndNullableApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**requiredAndNullableGet**](RequiredAndNullableApi.md#requiredAndNullableGet) | **GET** /requiredAndNullable |  |
| [**requiredAndNullableGetWithHttpInfo**](RequiredAndNullableApi.md#requiredAndNullableGetWithHttpInfo) | **GET** /requiredAndNullable |  |
| [**requiredAndNullablePost**](RequiredAndNullableApi.md#requiredAndNullablePost) | **POST** /requiredAndNullable |  |
| [**requiredAndNullablePostWithHttpInfo**](RequiredAndNullableApi.md#requiredAndNullablePostWithHttpInfo) | **POST** /requiredAndNullable |  |



## requiredAndNullableGet

> void requiredAndNullableGet(filter)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.RequiredAndNullableApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        RequiredAndNullableApi apiInstance = new RequiredAndNullableApi(defaultClient);
        String filter = "filter_example"; // String | 
        try {
            apiInstance.requiredAndNullableGet(filter);
        } catch (ApiException e) {
            System.err.println("Exception when calling RequiredAndNullableApi#requiredAndNullableGet");
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
| **filter** | **String**|  | [optional] |

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

## requiredAndNullableGetWithHttpInfo

> ApiResponse<Void> requiredAndNullableGetWithHttpInfo(filter)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.RequiredAndNullableApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        RequiredAndNullableApi apiInstance = new RequiredAndNullableApi(defaultClient);
        String filter = "filter_example"; // String | 
        try {
            ApiResponse<Void> response = apiInstance.requiredAndNullableGetWithHttpInfo(filter);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
        } catch (ApiException e) {
            System.err.println("Exception when calling RequiredAndNullableApi#requiredAndNullableGet");
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
| **filter** | **String**|  | [optional] |

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


## requiredAndNullablePost

> RequiredAndNullable requiredAndNullablePost(requiredAndNullable)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.RequiredAndNullableApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        RequiredAndNullableApi apiInstance = new RequiredAndNullableApi(defaultClient);
        RequiredAndNullable requiredAndNullable = new RequiredAndNullable(); // RequiredAndNullable | bodyWithRequiredAndNullableAttributes
        try {
            RequiredAndNullable result = apiInstance.requiredAndNullablePost(requiredAndNullable);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling RequiredAndNullableApi#requiredAndNullablePost");
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

## requiredAndNullablePostWithHttpInfo

> ApiResponse<RequiredAndNullable> requiredAndNullablePostWithHttpInfo(requiredAndNullable)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.RequiredAndNullableApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        RequiredAndNullableApi apiInstance = new RequiredAndNullableApi(defaultClient);
        RequiredAndNullable requiredAndNullable = new RequiredAndNullable(); // RequiredAndNullable | bodyWithRequiredAndNullableAttributes
        try {
            ApiResponse<RequiredAndNullable> response = apiInstance.requiredAndNullablePostWithHttpInfo(requiredAndNullable);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling RequiredAndNullableApi#requiredAndNullablePost");
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

