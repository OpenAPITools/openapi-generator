# Org.OpenAPITools.Api.PathApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**TestsPathStringPathStringIntegerPathInteger**](PathApi.md#testspathstringpathstringintegerpathinteger) | **GET** /path/string/{path_string}/integer/{path_integer} | Test path parameter(s) |

<a id="testspathstringpathstringintegerpathinteger"></a>
# **TestsPathStringPathStringIntegerPathInteger**
> string TestsPathStringPathStringIntegerPathInteger (string pathString, int pathInteger)

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
    public class TestsPathStringPathStringIntegerPathIntegerExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new PathApi(config);
            var pathString = "pathString_example";  // string | 
            var pathInteger = 56;  // int | 

            try
            {
                // Test path parameter(s)
                string result = apiInstance.TestsPathStringPathStringIntegerPathInteger(pathString, pathInteger);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling PathApi.TestsPathStringPathStringIntegerPathInteger: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestsPathStringPathStringIntegerPathIntegerWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test path parameter(s)
    ApiResponse<string> response = apiInstance.TestsPathStringPathStringIntegerPathIntegerWithHttpInfo(pathString, pathInteger);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling PathApi.TestsPathStringPathStringIntegerPathIntegerWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pathString** | **string** |  |  |
| **pathInteger** | **int** |  |  |

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

