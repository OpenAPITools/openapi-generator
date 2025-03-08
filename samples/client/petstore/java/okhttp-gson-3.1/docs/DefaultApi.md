# DefaultApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fakeWebhooksSourcesDeletedPost**](DefaultApi.md#fakeWebhooksSourcesDeletedPost) | **POST** /fake/webhooks/sources/deleted |  |


<a id="fakeWebhooksSourcesDeletedPost"></a>
# **fakeWebhooksSourcesDeletedPost**
> fakeWebhooksSourcesDeletedPost(fakeWebhooksSourcesDeletedPostRequest)



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    DefaultApi apiInstance = new DefaultApi(defaultClient);
    FakeWebhooksSourcesDeletedPostRequest fakeWebhooksSourcesDeletedPostRequest = new FakeWebhooksSourcesDeletedPostRequest(); // FakeWebhooksSourcesDeletedPostRequest | 
    try {
      apiInstance.fakeWebhooksSourcesDeletedPost(fakeWebhooksSourcesDeletedPostRequest);
    } catch (ApiException e) {
      System.err.println("Exception when calling DefaultApi#fakeWebhooksSourcesDeletedPost");
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
| **fakeWebhooksSourcesDeletedPostRequest** | [**FakeWebhooksSourcesDeletedPostRequest**](FakeWebhooksSourcesDeletedPostRequest.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **405** | Invalid input |  -  |

