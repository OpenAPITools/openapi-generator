# Org.OpenAPITools.Api.PathApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#testspathstringpathstringintegerpathintegerenumnonrefstringpathenumrefstringpath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s) |

<a id="testspathstringpathstringintegerpathintegerenumnonrefstringpathenumrefstringpath"></a>
# **TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> string TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath (string pathString, int pathInteger, string enumNonrefStringPath, StringEnumRef enumRefStringPath)

Test path parameter(s)

Test path parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new PathApi(config);
            var pathString = "pathString_example";  // string | 
            var pathInteger = 56;  // int | 
            var enumNonrefStringPath = "success";  // string | 
            var enumRefStringPath = new StringEnumRef(); // StringEnumRef | 

            try
            {
                // Test path parameter(s)
                string result = apiInstance.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling PathApi.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test path parameter(s)
    ApiResponse<string> response = apiInstance.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling PathApi.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pathString** | **string** |  |  |
| **pathInteger** | **int** |  |  |
| **enumNonrefStringPath** | **string** |  |  |
| **enumRefStringPath** | [**StringEnumRef**](StringEnumRef.md) |  |  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

