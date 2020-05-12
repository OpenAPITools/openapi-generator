# Org.OpenAPITools.Api.FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestClassname**](FakeClassnameTags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case



## TestClassname

> ModelClient TestClassname (ModelClient body)

To test class name in snake case

To test class name in snake case

### Example

```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestClassnameExample
    {
        public static void Main()
        {
            Configuration.Default.BasePath = "http://petstore.swagger.io:80/v2";
            // Configure API key authorization: api_key_query
            Configuration.Default.AddApiKey("api_key_query", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.AddApiKeyPrefix("api_key_query", "Bearer");

            var apiInstance = new FakeClassnameTags123Api(Configuration.Default);
            var body = new ModelClient(); // ModelClient | client model

            try
            {
                // To test class name in snake case
                ModelClient result = apiInstance.TestClassname(body);
                Debug.WriteLine(result);
            }
            catch (ApiException e)
            {
                Debug.Print("Exception when calling FakeClassnameTags123Api.TestClassname: " + e.Message );
                Debug.Print("Status Code: "+ e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ModelClient**](ModelClient.md)| client model | 

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#)
[[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

