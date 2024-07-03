# DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fooGet**](DefaultApi.md#fooGet) | **GET** /foo |  |



## fooGet

> FooGetDefaultResponse fooGet()



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.auth.*;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");
        
        // Configure API key authorization: global_api_key_cookie
        ApiKeyAuth global_api_key_cookie = (ApiKeyAuth) defaultClient.getAuthentication("global_api_key_cookie");
        global_api_key_cookie.setApiKey("YOUR API KEY");
        // Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
        //global_api_key_cookie.setApiKeyPrefix("Token");

        // Configure API key authorization: global_api_key_header
        ApiKeyAuth global_api_key_header = (ApiKeyAuth) defaultClient.getAuthentication("global_api_key_header");
        global_api_key_header.setApiKey("YOUR API KEY");
        // Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
        //global_api_key_header.setApiKeyPrefix("Token");

        DefaultApi apiInstance = new DefaultApi(defaultClient);
        try {
            FooGetDefaultResponse result = apiInstance.fooGet();
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling DefaultApi#fooGet");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

[global_api_key_cookie](../README.md#global_api_key_cookie), [global_api_key_header](../README.md#global_api_key_header)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

