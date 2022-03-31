# Org.OpenAPITools.Api.MultipartApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**MultipartArray**](MultipartApi.md#multipartarray) | **POST** /multipart-array | 
[**MultipartMixed**](MultipartApi.md#multipartmixed) | **POST** /multipart-mixed | 
[**MultipartSingle**](MultipartApi.md#multipartsingle) | **POST** /multipart-single | 


<a name="multipartarray"></a>
# **MultipartArray**
> void MultipartArray (List<System.IO.Stream> files = null)



MultipartFile array test

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class MultipartArrayExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost";
            var apiInstance = new MultipartApi(config);
            var files = new List<System.IO.Stream>(); // List<System.IO.Stream> | Many files (optional) 

            try
            {
                apiInstance.MultipartArray(files);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling MultipartApi.MultipartArray: " + e.Message );
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
 **files** | **List&lt;System.IO.Stream&gt;**| Many files | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **204** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="multipartmixed"></a>
# **MultipartMixed**
> void MultipartMixed (MultipartMixedStatus status, System.IO.Stream file, MultipartMixedMarker marker = null)



Mixed MultipartFile test

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class MultipartMixedExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost";
            var apiInstance = new MultipartApi(config);
            var status = (MultipartMixedStatus) "ALLOWED";  // MultipartMixedStatus | 
            var file = new System.IO.MemoryStream(System.IO.File.ReadAllBytes("/path/to/file.txt"));  // System.IO.Stream | a file
            var marker = new MultipartMixedMarker(); // MultipartMixedMarker |  (optional) 

            try
            {
                apiInstance.MultipartMixed(status, file, marker);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling MultipartApi.MultipartMixed: " + e.Message );
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
 **status** | **MultipartMixedStatus**|  | 
 **file** | **System.IO.Stream****System.IO.Stream**| a file | 
 **marker** | [**MultipartMixedMarker**](MultipartMixedMarker.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **204** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="multipartsingle"></a>
# **MultipartSingle**
> void MultipartSingle (System.IO.Stream file = null)



Single MultipartFile test

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class MultipartSingleExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost";
            var apiInstance = new MultipartApi(config);
            var file = new System.IO.MemoryStream(System.IO.File.ReadAllBytes("/path/to/file.txt"));  // System.IO.Stream | One file (optional) 

            try
            {
                apiInstance.MultipartSingle(file);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling MultipartApi.MultipartSingle: " + e.Message );
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
 **file** | **System.IO.Stream****System.IO.Stream**| One file | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **204** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

