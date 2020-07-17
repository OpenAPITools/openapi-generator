# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testPost**](DefaultApi.md#testPost) | **POST** /test | 



## testPost

> MySchemaNameCharacters testPost(mySchemaNameCharacters)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.model.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        DefaultApi apiInstance = new DefaultApi(defaultClient);
        MySchemaNameCharacters mySchemaNameCharacters = new MySchemaNameCharacters(); // MySchemaNameCharacters | 
        try {
            MySchemaNameCharacters result = apiInstance.testPost(mySchemaNameCharacters);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling DefaultApi#testPost");
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
 **mySchemaNameCharacters** | [**MySchemaNameCharacters**](MySchemaNameCharacters.md)|  | [optional]

### Return type

[**MySchemaNameCharacters**](MySchemaNameCharacters.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | the response |  -  |

