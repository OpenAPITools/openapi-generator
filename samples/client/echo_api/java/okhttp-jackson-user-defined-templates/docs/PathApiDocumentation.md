# PathApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s) |


<a id="testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath"></a>
# **testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> String testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath)

Test path parameter(s)

Test path parameter(s)

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.PathApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://localhost:3000");

    PathApi apiInstance = new PathApi(defaultClient);
    String pathString = "pathString_example"; // String | 
    Integer pathInteger = 56; // Integer | 
    String enumNonrefStringPath = "success"; // String | 
    StringEnumRef enumRefStringPath = StringEnumRef.fromValue("success"); // StringEnumRef | 
    try {
      String result = apiInstance.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling PathApi#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
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
| **pathString** | **String**|  | |
| **pathInteger** | **Integer**|  | |
| **enumNonrefStringPath** | **String**|  | [enum: success, failure, unclassified] |
| **enumRefStringPath** | [**StringEnumRef**](.md)|  | [enum: success, failure, unclassified] |

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

