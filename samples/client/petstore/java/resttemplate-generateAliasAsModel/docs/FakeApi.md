# FakeApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testAdditionalPropertiesReference**](FakeApi.md#testAdditionalPropertiesReference) | **POST** /fake/additionalProperties-reference | test referenced additionalProperties |



## testAdditionalPropertiesReference

> testAdditionalPropertiesReference(additionalProperties)

test referenced additionalProperties



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost");

        FakeApi apiInstance = new FakeApi(defaultClient);
        AdditionalProperties additionalProperties = new AdditionalProperties(); // AdditionalProperties | request body
        try {
            apiInstance.testAdditionalPropertiesReference(additionalProperties);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testAdditionalPropertiesReference");
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
| **additionalProperties** | [**AdditionalProperties**](AdditionalProperties.md)| request body | |

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

