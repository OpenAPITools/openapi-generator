# AuthApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testAuthHttpBasic**](AuthApi.md#testAuthHttpBasic) | **POST** /auth/http/basic | To test HTTP basic authentication |



## testAuthHttpBasic

> String testAuthHttpBasic()

To test HTTP basic authentication

To test HTTP basic authentication

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.auth.*;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AuthApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");
        
        // Configure HTTP basic authorization: http_auth
        HttpBasicAuth http_auth = (HttpBasicAuth) defaultClient.getAuthentication("http_auth");
        http_auth.setUsername("YOUR USERNAME");
        http_auth.setPassword("YOUR PASSWORD");

        AuthApi apiInstance = new AuthApi(defaultClient);
        try {
            String result = apiInstance.testAuthHttpBasic();
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling AuthApi#testAuthHttpBasic");
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

**String**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

