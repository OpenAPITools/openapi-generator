# HeaderApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testHeaderIntegerBooleanString**](HeaderApi.md#testHeaderIntegerBooleanString) | **GET** /header/integer/boolean/string | Test header parameter(s) |


<a id="testHeaderIntegerBooleanString"></a>
# **testHeaderIntegerBooleanString**
> String testHeaderIntegerBooleanString(integerHeader, booleanHeader, stringHeader)

Test header parameter(s)

Test header parameter(s)

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.HeaderApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://localhost:3000");

    HeaderApi apiInstance = new HeaderApi(defaultClient);
    Integer integerHeader = 56; // Integer | 
    Boolean booleanHeader = true; // Boolean | 
    String stringHeader = "stringHeader_example"; // String | 
    try {
      String result = apiInstance.testHeaderIntegerBooleanString(integerHeader, booleanHeader, stringHeader);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling HeaderApi#testHeaderIntegerBooleanString");
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
| **integerHeader** | **Integer**|  | [optional] |
| **booleanHeader** | **Boolean**|  | [optional] |
| **stringHeader** | **String**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

