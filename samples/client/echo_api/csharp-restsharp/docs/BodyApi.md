# Org.OpenAPITools.Api.BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**TestBinaryGif**](BodyApi.md#testbinarygif) | **POST** /binary/gif | Test binary (gif) response body |
| [**TestBodyApplicationOctetstreamBinary**](BodyApi.md#testbodyapplicationoctetstreambinary) | **POST** /body/application/octetstream/binary | Test body parameter(s) |
| [**TestBodyMultipartFormdataArrayOfBinary**](BodyApi.md#testbodymultipartformdataarrayofbinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime |
| [**TestEchoBodyFreeFormObjectResponseString**](BodyApi.md#testechobodyfreeformobjectresponsestring) | **POST** /echo/body/FreeFormObject/response_string | Test free form object |
| [**TestEchoBodyPet**](BodyApi.md#testechobodypet) | **POST** /echo/body/Pet | Test body parameter(s) |
| [**TestEchoBodyPetResponseString**](BodyApi.md#testechobodypetresponsestring) | **POST** /echo/body/Pet/response_string | Test empty response body |
| [**TestEchoBodyTagResponseString**](BodyApi.md#testechobodytagresponsestring) | **POST** /echo/body/Tag/response_string | Test empty json (request body) |

<a id="testbinarygif"></a>
# **TestBinaryGif**
> System.IO.Stream TestBinaryGif ()

Test binary (gif) response body

Test binary (gif) response body

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestBinaryGifExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);

            try
            {
                // Test binary (gif) response body
                System.IO.Stream result = apiInstance.TestBinaryGif();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestBinaryGif: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestBinaryGifWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test binary (gif) response body
    ApiResponse<System.IO.Stream> response = apiInstance.TestBinaryGifWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestBinaryGifWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

**System.IO.Stream**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testbodyapplicationoctetstreambinary"></a>
# **TestBodyApplicationOctetstreamBinary**
> string TestBodyApplicationOctetstreamBinary (System.IO.Stream? body = null)

Test body parameter(s)

Test body parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestBodyApplicationOctetstreamBinaryExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);
            var body = new System.IO.MemoryStream(System.IO.File.ReadAllBytes("/path/to/file.txt"));  // System.IO.Stream? |  (optional) 

            try
            {
                // Test body parameter(s)
                string result = apiInstance.TestBodyApplicationOctetstreamBinary(body);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestBodyApplicationOctetstreamBinary: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestBodyApplicationOctetstreamBinaryWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test body parameter(s)
    ApiResponse<string> response = apiInstance.TestBodyApplicationOctetstreamBinaryWithHttpInfo(body);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestBodyApplicationOctetstreamBinaryWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **body** | **System.IO.Stream?****System.IO.Stream?** |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testbodymultipartformdataarrayofbinary"></a>
# **TestBodyMultipartFormdataArrayOfBinary**
> string TestBodyMultipartFormdataArrayOfBinary (List<System.IO.Stream> files)

Test array of binary in multipart mime

Test array of binary in multipart mime

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestBodyMultipartFormdataArrayOfBinaryExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);
            var files = new List<System.IO.Stream>(); // List<System.IO.Stream> | 

            try
            {
                // Test array of binary in multipart mime
                string result = apiInstance.TestBodyMultipartFormdataArrayOfBinary(files);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestBodyMultipartFormdataArrayOfBinary: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestBodyMultipartFormdataArrayOfBinaryWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test array of binary in multipart mime
    ApiResponse<string> response = apiInstance.TestBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestBodyMultipartFormdataArrayOfBinaryWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **files** | **List&lt;System.IO.Stream&gt;** |  |  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testechobodyfreeformobjectresponsestring"></a>
# **TestEchoBodyFreeFormObjectResponseString**
> string TestEchoBodyFreeFormObjectResponseString (Object? body = null)

Test free form object

Test free form object

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEchoBodyFreeFormObjectResponseStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);
            var body = null;  // Object? | Free form object (optional) 

            try
            {
                // Test free form object
                string result = apiInstance.TestEchoBodyFreeFormObjectResponseString(body);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestEchoBodyFreeFormObjectResponseString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEchoBodyFreeFormObjectResponseStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test free form object
    ApiResponse<string> response = apiInstance.TestEchoBodyFreeFormObjectResponseStringWithHttpInfo(body);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestEchoBodyFreeFormObjectResponseStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **body** | **Object?** | Free form object | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testechobodypet"></a>
# **TestEchoBodyPet**
> Pet TestEchoBodyPet (Pet? pet = null)

Test body parameter(s)

Test body parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEchoBodyPetExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);
            var pet = new Pet?(); // Pet? | Pet object that needs to be added to the store (optional) 

            try
            {
                // Test body parameter(s)
                Pet result = apiInstance.TestEchoBodyPet(pet);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestEchoBodyPet: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEchoBodyPetWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test body parameter(s)
    ApiResponse<Pet> response = apiInstance.TestEchoBodyPetWithHttpInfo(pet);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestEchoBodyPetWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pet** | [**Pet?**](Pet?.md) | Pet object that needs to be added to the store | [optional]  |

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testechobodypetresponsestring"></a>
# **TestEchoBodyPetResponseString**
> string TestEchoBodyPetResponseString (Pet? pet = null)

Test empty response body

Test empty response body

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEchoBodyPetResponseStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);
            var pet = new Pet?(); // Pet? | Pet object that needs to be added to the store (optional) 

            try
            {
                // Test empty response body
                string result = apiInstance.TestEchoBodyPetResponseString(pet);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestEchoBodyPetResponseString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEchoBodyPetResponseStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test empty response body
    ApiResponse<string> response = apiInstance.TestEchoBodyPetResponseStringWithHttpInfo(pet);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestEchoBodyPetResponseStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pet** | [**Pet?**](Pet?.md) | Pet object that needs to be added to the store | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testechobodytagresponsestring"></a>
# **TestEchoBodyTagResponseString**
> string TestEchoBodyTagResponseString (Tag? tag = null)

Test empty json (request body)

Test empty json (request body)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEchoBodyTagResponseStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new BodyApi(config);
            var tag = new Tag?(); // Tag? | Tag object (optional) 

            try
            {
                // Test empty json (request body)
                string result = apiInstance.TestEchoBodyTagResponseString(tag);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling BodyApi.TestEchoBodyTagResponseString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEchoBodyTagResponseStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test empty json (request body)
    ApiResponse<string> response = apiInstance.TestEchoBodyTagResponseStringWithHttpInfo(tag);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling BodyApi.TestEchoBodyTagResponseStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **tag** | [**Tag?**](Tag?.md) | Tag object | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

