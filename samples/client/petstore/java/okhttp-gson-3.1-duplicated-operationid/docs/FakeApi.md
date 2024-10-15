# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**getPetById_0**](FakeApi.md#getPetById_0) | **GET** /fake/duplicated/operationId |  |


<a id="getPetById_0"></a>
# **getPetById_0**
> Object getPetById_0()



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
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    try {
      Object result = apiInstance.getPetById_0();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#getPetById_0");
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

**Object**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** |  |  -  |

