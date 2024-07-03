# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags |



## call123testSpecialTags

> Client call123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.auth.*;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AnotherFakeApi;

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

        AnotherFakeApi apiInstance = new AnotherFakeApi(defaultClient);
        Client client = new Client(); // Client | client model
        try {
            Client result = apiInstance.call123testSpecialTags(client);
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


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **client** | [**Client**](Client.md)| client model | |

### Return type

[**Client**](Client.md)

### Authorization

[global_api_key_cookie](../README.md#global_api_key_cookie), [global_api_key_header](../README.md#global_api_key_header)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

