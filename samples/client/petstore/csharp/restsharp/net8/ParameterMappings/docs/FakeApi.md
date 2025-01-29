# Org.OpenAPITools.Api.FakeApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**GetParameterNameMapping**](FakeApi.md#getparameternamemapping) | **GET** /fake/parameter-name-mapping | parameter name mapping test |

<a id="getparameternamemapping"></a>
# **GetParameterNameMapping**
> Env GetParameterNameMapping (long UnderscoreType, string type, string TypeWithUnderscore, string httpDebugOption)

parameter name mapping test

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class GetParameterNameMappingExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost";
            var apiInstance = new FakeApi(config);
            var UnderscoreType = 789L;  // long | _type
            var type = "type_example";  // string | type
            var TypeWithUnderscore = "TypeWithUnderscore_example";  // string | type_
            var httpDebugOption = "httpDebugOption_example";  // string | http debug option (to test parameter naming option)

            try
            {
                // parameter name mapping test
                Env result = apiInstance.GetParameterNameMapping(UnderscoreType, type, TypeWithUnderscore, httpDebugOption);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.GetParameterNameMapping: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the GetParameterNameMappingWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // parameter name mapping test
    ApiResponse<Env> response = apiInstance.GetParameterNameMappingWithHttpInfo(UnderscoreType, type, TypeWithUnderscore, httpDebugOption);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.GetParameterNameMappingWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **UnderscoreType** | **long** | _type |  |
| **type** | **string** | type |  |
| **TypeWithUnderscore** | **string** | type_ |  |
| **httpDebugOption** | **string** | http debug option (to test parameter naming option) |  |

### Return type

[**Env**](Env.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

