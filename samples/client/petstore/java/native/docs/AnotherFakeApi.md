# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags
[**call123testSpecialTagsWithHttpInfo**](AnotherFakeApi.md#call123testSpecialTagsWithHttpInfo) | **PATCH** /another-fake/dummy | To test special tags



## call123testSpecialTags

> Client call123testSpecialTags(body)

To test special tags

To test special tags and operation ID starting with number

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AnotherFakeApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        AnotherFakeApi apiInstance = new AnotherFakeApi(defaultClient);
        Client body = new Client(); // Client | client model
        try {
            Client result = apiInstance.call123testSpecialTags(body);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling AnotherFakeApi#call123testSpecialTags");
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
 **body** | [**Client**](Client.md)| client model |

### Return type

[**Client**](Client.md)


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

## call123testSpecialTagsWithHttpInfo

> ApiResponse<Client> call123testSpecialTags call123testSpecialTagsWithHttpInfo(body)

To test special tags

To test special tags and operation ID starting with number

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AnotherFakeApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        AnotherFakeApi apiInstance = new AnotherFakeApi(defaultClient);
        Client body = new Client(); // Client | client model
        try {
            ApiResponse<Client> response = apiInstance.call123testSpecialTagsWithHttpInfo(body);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling AnotherFakeApi#call123testSpecialTags");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model |

### Return type

ApiResponse<[**Client**](Client.md)>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

