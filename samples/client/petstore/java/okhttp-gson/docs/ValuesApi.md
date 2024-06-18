# ValuesApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**getSomeValues**](ValuesApi.md#getSomeValues) | **GET** /values | Get some primitive variable values |


<a id="getSomeValues"></a>
# **getSomeValues**
> Variable getSomeValues()

Get some primitive variable values



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.ValuesApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    ValuesApi apiInstance = new ValuesApi(defaultClient);
    try {
      Variable result = apiInstance.getSomeValues();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling ValuesApi#getSomeValues");
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

[**Variable**](Variable.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid Value |  -  |

