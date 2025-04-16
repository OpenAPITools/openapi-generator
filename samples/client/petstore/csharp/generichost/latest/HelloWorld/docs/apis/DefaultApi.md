# Org.OpenAPITools.Api.DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**HelloWorldPost**](DefaultApi.md#helloworldpost) | **POST** /helloWorld | Example webhook |

<a id="helloworldpost"></a>
# **HelloWorldPost**
> void HelloWorldPost (HelloWorldPostRequest helloWorldPostRequest = null)

Example webhook

Send when an example is needed

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class HelloWorldPostExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost";
            var apiInstance = new DefaultApi(config);
            var helloWorldPostRequest = new HelloWorldPostRequest(); // HelloWorldPostRequest | Contains the details of the hello world webhook (optional) 

            try
            {
                // Example webhook
                apiInstance.HelloWorldPost(helloWorldPostRequest);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.HelloWorldPost: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the HelloWorldPostWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Example webhook
    apiInstance.HelloWorldPostWithHttpInfo(helloWorldPostRequest);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.HelloWorldPostWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **helloWorldPostRequest** | [**HelloWorldPostRequest**](HelloWorldPostRequest.md) | Contains the details of the hello world webhook | [optional]  |

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
| **204** | Webhook processed |  -  |
| **500** | Webhook not processed |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

