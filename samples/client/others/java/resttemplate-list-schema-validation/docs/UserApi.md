# UserApi

All URIs are relative to *http://api.example.xyz/v1*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**userGet**](UserApi.md#userGet) | **GET** /user |  |



## userGet

> userGet(username)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.UserApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://api.example.xyz/v1");

        UserApi apiInstance = new UserApi(defaultClient);
        List<@Pattern(regexp = "^[a-zA-Z0-9]$")String> username = Arrays.asList(); // List<@Pattern(regexp = "^[a-zA-Z0-9]$")String> | The name of the user
        try {
            apiInstance.userGet(username);
        } catch (ApiException e) {
            System.err.println("Exception when calling UserApi#userGet");
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
| **username** | [**List&lt;@Pattern(regexp &#x3D; &quot;^[a-zA-Z0-9]$&quot;)String&gt;**](String.md)| The name of the user | |

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
| **200** | OK |  -  |

