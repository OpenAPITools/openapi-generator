# Org.OpenAPITools.Api.EnvironmentApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**ApiV1EnvironmentsGet**](EnvironmentApi.md#apiv1environmentsget) | **GET** /api/v1/environments |  |
| [**ApiV1EnvironmentsIdDelete**](EnvironmentApi.md#apiv1environmentsiddelete) | **DELETE** /api/v1/environments/{id} |  |
| [**ApiV1EnvironmentsIdGet**](EnvironmentApi.md#apiv1environmentsidget) | **GET** /api/v1/environments/{id} |  |
| [**ApiV1EnvironmentsIdPut**](EnvironmentApi.md#apiv1environmentsidput) | **PUT** /api/v1/environments/{id} |  |
| [**ApiV1EnvironmentsPost**](EnvironmentApi.md#apiv1environmentspost) | **POST** /api/v1/environments |  |

<a id="apiv1environmentsget"></a>
# **ApiV1EnvironmentsGet**
> List&lt;Environment&gt; ApiV1EnvironmentsGet ()



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class ApiV1EnvironmentsGetExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new EnvironmentApi(config);

            try
            {
                List<Environment> result = apiInstance.ApiV1EnvironmentsGet();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsGet: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the ApiV1EnvironmentsGetWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<List<Environment>> response = apiInstance.ApiV1EnvironmentsGetWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsGetWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

[**List&lt;Environment&gt;**](Environment.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain, application/json, text/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="apiv1environmentsiddelete"></a>
# **ApiV1EnvironmentsIdDelete**
> void ApiV1EnvironmentsIdDelete (Guid id)



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class ApiV1EnvironmentsIdDeleteExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new EnvironmentApi(config);
            var id = "id_example";  // Guid | 

            try
            {
                apiInstance.ApiV1EnvironmentsIdDelete(id);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsIdDelete: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the ApiV1EnvironmentsIdDeleteWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.ApiV1EnvironmentsIdDeleteWithHttpInfo(id);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsIdDeleteWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **id** | **Guid** |  |  |

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
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="apiv1environmentsidget"></a>
# **ApiV1EnvironmentsIdGet**
> Environment ApiV1EnvironmentsIdGet (Guid id)



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class ApiV1EnvironmentsIdGetExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new EnvironmentApi(config);
            var id = "id_example";  // Guid | 

            try
            {
                Environment result = apiInstance.ApiV1EnvironmentsIdGet(id);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsIdGet: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the ApiV1EnvironmentsIdGetWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<Environment> response = apiInstance.ApiV1EnvironmentsIdGetWithHttpInfo(id);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsIdGetWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **id** | **Guid** |  |  |

### Return type

[**Environment**](Environment.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain, application/json, text/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="apiv1environmentsidput"></a>
# **ApiV1EnvironmentsIdPut**
> void ApiV1EnvironmentsIdPut (Guid id, Environment environment = null)



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class ApiV1EnvironmentsIdPutExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new EnvironmentApi(config);
            var id = "id_example";  // Guid | 
            var environment = new Environment(); // Environment |  (optional) 

            try
            {
                apiInstance.ApiV1EnvironmentsIdPut(id, environment);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsIdPut: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the ApiV1EnvironmentsIdPutWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.ApiV1EnvironmentsIdPutWithHttpInfo(id, environment);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsIdPutWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **id** | **Guid** |  |  |
| **environment** | [**Environment**](Environment.md) |  | [optional]  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, text/json, application/*+json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="apiv1environmentspost"></a>
# **ApiV1EnvironmentsPost**
> Environment ApiV1EnvironmentsPost (Environment environment = null)



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class ApiV1EnvironmentsPostExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new EnvironmentApi(config);
            var environment = new Environment(); // Environment |  (optional) 

            try
            {
                Environment result = apiInstance.ApiV1EnvironmentsPost(environment);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsPost: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the ApiV1EnvironmentsPostWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<Environment> response = apiInstance.ApiV1EnvironmentsPostWithHttpInfo(environment);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling EnvironmentApi.ApiV1EnvironmentsPostWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **environment** | [**Environment**](Environment.md) |  | [optional]  |

### Return type

[**Environment**](Environment.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, text/json, application/*+json
 - **Accept**: text/plain, application/json, text/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

