# UseSourceGeneration.Api.DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**FooGet**](DefaultApi.md#fooget) | **GET** /foo |  |
| [**GetCountry**](DefaultApi.md#getcountry) | **POST** /country |  |
| [**Hello**](DefaultApi.md#hello) | **GET** /hello | Hello |
| [**RolesReportGet**](DefaultApi.md#rolesreportget) | **GET** /roles/report |  |
| [**Test**](DefaultApi.md#test) | **GET** /test | Retrieve an existing Notificationtest&#39;s Elements |

<a id="fooget"></a>
# **FooGet**
> FooGetDefaultResponse FooGet ()



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class FooGetExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new DefaultApi(config);

            try
            {
                FooGetDefaultResponse result = apiInstance.FooGet();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.FooGet: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the FooGetWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<FooGetDefaultResponse> response = apiInstance.FooGetWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.FooGetWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getcountry"></a>
# **GetCountry**
> void GetCountry (string country)



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class GetCountryExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new DefaultApi(config);
            var country = "country_example";  // string | 

            try
            {
                apiInstance.GetCountry(country);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.GetCountry: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the GetCountryWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.GetCountryWithHttpInfo(country);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.GetCountryWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **country** | **string** |  |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="hello"></a>
# **Hello**
> List&lt;Guid&gt; Hello ()

Hello

Hello

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class HelloExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new DefaultApi(config);

            try
            {
                // Hello
                List<Guid> result = apiInstance.Hello();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.Hello: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the HelloWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Hello
    ApiResponse<List<Guid>> response = apiInstance.HelloWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.HelloWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

**List<Guid>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | UUIDs |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="rolesreportget"></a>
# **RolesReportGet**
> List&lt;List&lt;RolesReportsHash&gt;&gt; RolesReportGet ()



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class RolesReportGetExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new DefaultApi(config);

            try
            {
                List<List<RolesReportsHash>> result = apiInstance.RolesReportGet();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.RolesReportGet: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the RolesReportGetWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<List<List<RolesReportsHash>>> response = apiInstance.RolesReportGetWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.RolesReportGetWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

**List<List<RolesReportsHash>>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | returns report |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="test"></a>
# **Test**
> NotificationtestGetElementsV1ResponseMPayload Test ()

Retrieve an existing Notificationtest's Elements

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class TestExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new DefaultApi(config);

            try
            {
                // Retrieve an existing Notificationtest's Elements
                NotificationtestGetElementsV1ResponseMPayload result = apiInstance.Test();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling DefaultApi.Test: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Retrieve an existing Notificationtest's Elements
    ApiResponse<NotificationtestGetElementsV1ResponseMPayload> response = apiInstance.TestWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling DefaultApi.TestWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

[**NotificationtestGetElementsV1ResponseMPayload**](NotificationtestGetElementsV1ResponseMPayload.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful response |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

