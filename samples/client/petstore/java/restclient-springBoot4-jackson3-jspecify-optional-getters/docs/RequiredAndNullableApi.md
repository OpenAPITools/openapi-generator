# RequiredAndNullableApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**requiredAndNullablePost**](RequiredAndNullableApi.md#requiredAndNullablePost) | **POST** /requiredAndNullable |  |



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

