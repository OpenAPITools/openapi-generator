# Org.OpenAPITools.Api.DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**OneOfArray**](DefaultApi.md#oneofarray) | **POST** /one-of-array |  |

<a id="oneofarray"></a>
# **OneOfArray**
> void OneOfArray (OneOfArrayRequest? oneOfArrayRequest = null)



Oneof array test

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class OneOfArrayExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost";
            var apiInstance = new DefaultApi(config);
            var oneOfArrayRequest = new OneOfArrayRequest?(); // OneOfArrayRequest? |  (optional) 

            try
            {
                apiInstance.OneOfArray(oneOfArrayRequest);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.OneOfArray: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the OneOfArrayWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.OneOfArrayWithHttpInfo(oneOfArrayRequest);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.OneOfArrayWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **oneOfArrayRequest** | [**OneOfArrayRequest?**](OneOfArrayRequest?.md) |  | [optional]  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **204** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

