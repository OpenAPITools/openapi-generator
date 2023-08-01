# Org.OpenAPITools.Api.HeaderApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**TestHeaderIntegerBooleanString**](HeaderApi.md#testheaderintegerbooleanstring) | **GET** /header/integer/boolean/string | Test header parameter(s) |

<a id="testheaderintegerbooleanstring"></a>
# **TestHeaderIntegerBooleanString**
> string TestHeaderIntegerBooleanString (int? integerHeader = null, bool? booleanHeader = null, string? stringHeader = null)

Test header parameter(s)

Test header parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestHeaderIntegerBooleanStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new HeaderApi(config);
            var integerHeader = 56;  // int? |  (optional) 
            var booleanHeader = true;  // bool? |  (optional) 
            var stringHeader = "stringHeader_example";  // string? |  (optional) 

            try
            {
                // Test header parameter(s)
                string result = apiInstance.TestHeaderIntegerBooleanString(integerHeader, booleanHeader, stringHeader);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling HeaderApi.TestHeaderIntegerBooleanString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestHeaderIntegerBooleanStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test header parameter(s)
    ApiResponse<string> response = apiInstance.TestHeaderIntegerBooleanStringWithHttpInfo(integerHeader, booleanHeader, stringHeader);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling HeaderApi.TestHeaderIntegerBooleanStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **integerHeader** | **int?** |  | [optional]  |
| **booleanHeader** | **bool?** |  | [optional]  |
| **stringHeader** | **string?** |  | [optional]  |

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

