# Org.OpenAPITools.Api.APIKEYSApi

All URIs are relative to *http://app.files.com/api/rest/v1*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**GetApiKeysId**](APIKEYSApi.md#getapikeysid) | **GET** /api_keys/{id} | Show API Key |

<a id="getapikeysid"></a>
# **GetApiKeysId**
> void GetApiKeysId (int id)

Show API Key

Show API Key

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class GetApiKeysIdExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://app.files.com/api/rest/v1";
            var apiInstance = new APIKEYSApi(config);
            var id = 56;  // int | Api Key ID.

            try
            {
                // Show API Key
                apiInstance.GetApiKeysId(id);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling APIKEYSApi.GetApiKeysId: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the GetApiKeysIdWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Show API Key
    apiInstance.GetApiKeysIdWithHttpInfo(id);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling APIKEYSApi.GetApiKeysIdWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **id** | **int** | Api Key ID. |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Bad Request |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

