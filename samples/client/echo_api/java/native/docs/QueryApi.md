# QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayStringWithHttpInfo**](QueryApi.md#testQueryStyleFormExplodeTrueArrayStringWithHttpInfo) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObjectWithHttpInfo**](QueryApi.md#testQueryStyleFormExplodeTrueObjectWithHttpInfo) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |



## testQueryStyleFormExplodeTrueArrayString

> String testQueryStyleFormExplodeTrueArrayString(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = new HashMap(); // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueArrayString(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueArrayString");
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
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

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

## testQueryStyleFormExplodeTrueArrayStringWithHttpInfo

> ApiResponse<String> testQueryStyleFormExplodeTrueArrayString testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = new HashMap(); // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueArrayString");
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
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleFormExplodeTrueObject

> String testQueryStyleFormExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new HashMap(); // Pet | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueObject(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

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

## testQueryStyleFormExplodeTrueObjectWithHttpInfo

> ApiResponse<String> testQueryStyleFormExplodeTrueObject testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new HashMap(); // Pet | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

