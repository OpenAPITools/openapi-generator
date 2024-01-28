# Org.OpenAPITools.Api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**FakeHealthGet**](FakeApi.md#fakehealthget) | **GET** /fake/health | Health check endpoint |
| [**FakeOuterBooleanSerialize**](FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean |  |
| [**FakeOuterCompositeSerialize**](FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite |  |
| [**FakeOuterNumberSerialize**](FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number |  |
| [**FakeOuterStringSerialize**](FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string |  |
| [**GetArrayOfEnums**](FakeApi.md#getarrayofenums) | **GET** /fake/array-of-enums | Array of Enums |
| [**TestBodyWithFileSchema**](FakeApi.md#testbodywithfileschema) | **PUT** /fake/body-with-file-schema |  |
| [**TestBodyWithQueryParams**](FakeApi.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params |  |
| [**TestClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model |
| [**TestEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  |
| [**TestEnumParameters**](FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters |
| [**TestGroupParameters**](FakeApi.md#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional) |
| [**TestInlineAdditionalProperties**](FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties |
| [**TestInlineFreeformAdditionalProperties**](FakeApi.md#testinlinefreeformadditionalproperties) | **POST** /fake/inline-freeform-additionalProperties | test inline free-form additionalProperties |
| [**TestJsonFormData**](FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data |
| [**TestQueryParameterCollectionFormat**](FakeApi.md#testqueryparametercollectionformat) | **PUT** /fake/test-query-parameters |  |

<a id="fakehealthget"></a>
# **FakeHealthGet**
> HealthCheckResult FakeHealthGet ()

Health check endpoint

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeHealthGetExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);

            try
            {
                // Health check endpoint
                HealthCheckResult result = apiInstance.FakeHealthGet();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.FakeHealthGet: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the FakeHealthGetWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Health check endpoint
    ApiResponse<HealthCheckResult> response = apiInstance.FakeHealthGetWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.FakeHealthGetWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="fakeouterbooleanserialize"></a>
# **FakeOuterBooleanSerialize**
> bool FakeOuterBooleanSerialize (bool? body = null)



Test serialization of outer boolean types

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterBooleanSerializeExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var body = true;  // bool? | Input boolean as post body (optional) 

            try
            {
                bool result = apiInstance.FakeOuterBooleanSerialize(body);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterBooleanSerialize: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the FakeOuterBooleanSerializeWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<bool> response = apiInstance.FakeOuterBooleanSerializeWithHttpInfo(body);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.FakeOuterBooleanSerializeWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **body** | **bool?** | Input boolean as post body | [optional]  |

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="fakeoutercompositeserialize"></a>
# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize (OuterComposite outerComposite = null)



Test serialization of object with outer number type

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterCompositeSerializeExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var outerComposite = new OuterComposite(); // OuterComposite | Input composite as post body (optional) 

            try
            {
                OuterComposite result = apiInstance.FakeOuterCompositeSerialize(outerComposite);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterCompositeSerialize: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the FakeOuterCompositeSerializeWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<OuterComposite> response = apiInstance.FakeOuterCompositeSerializeWithHttpInfo(outerComposite);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.FakeOuterCompositeSerializeWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **outerComposite** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | [optional]  |

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output composite |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="fakeouternumberserialize"></a>
# **FakeOuterNumberSerialize**
> decimal FakeOuterNumberSerialize (decimal? body = null)



Test serialization of outer number types

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterNumberSerializeExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var body = 8.14D;  // decimal? | Input number as post body (optional) 

            try
            {
                decimal result = apiInstance.FakeOuterNumberSerialize(body);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterNumberSerialize: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the FakeOuterNumberSerializeWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<decimal> response = apiInstance.FakeOuterNumberSerializeWithHttpInfo(body);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.FakeOuterNumberSerializeWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **body** | **decimal?** | Input number as post body | [optional]  |

### Return type

**decimal**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output number |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="fakeouterstringserialize"></a>
# **FakeOuterStringSerialize**
> string FakeOuterStringSerialize (Guid requiredStringUuid, string body = null)



Test serialization of outer string types

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterStringSerializeExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var requiredStringUuid = "requiredStringUuid_example";  // Guid | Required UUID String
            var body = "body_example";  // string | Input string as post body (optional) 

            try
            {
                string result = apiInstance.FakeOuterStringSerialize(requiredStringUuid, body);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterStringSerialize: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the FakeOuterStringSerializeWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    ApiResponse<string> response = apiInstance.FakeOuterStringSerializeWithHttpInfo(requiredStringUuid, body);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.FakeOuterStringSerializeWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requiredStringUuid** | **Guid** | Required UUID String |  |
| **body** | **string** | Input string as post body | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="getarrayofenums"></a>
# **GetArrayOfEnums**
> List&lt;OuterEnum&gt; GetArrayOfEnums ()

Array of Enums

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class GetArrayOfEnumsExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);

            try
            {
                // Array of Enums
                List<OuterEnum> result = apiInstance.GetArrayOfEnums();
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.GetArrayOfEnums: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the GetArrayOfEnumsWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Array of Enums
    ApiResponse<List<OuterEnum>> response = apiInstance.GetArrayOfEnumsWithHttpInfo();
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.GetArrayOfEnumsWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters
This endpoint does not need any parameter.
### Return type

[**List&lt;OuterEnum&gt;**](OuterEnum.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Got named array of enums |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testbodywithfileschema"></a>
# **TestBodyWithFileSchema**
> void TestBodyWithFileSchema (FileSchemaTestClass fileSchemaTestClass)



For this test, the body for this request much reference a schema named `File`.

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestBodyWithFileSchemaExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var fileSchemaTestClass = new FileSchemaTestClass(); // FileSchemaTestClass | 

            try
            {
                apiInstance.TestBodyWithFileSchema(fileSchemaTestClass);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestBodyWithFileSchema: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestBodyWithFileSchemaWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.TestBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestBodyWithFileSchemaWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md) |  |  |

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
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testbodywithqueryparams"></a>
# **TestBodyWithQueryParams**
> void TestBodyWithQueryParams (string query, User user)



### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestBodyWithQueryParamsExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var query = "query_example";  // string | 
            var user = new User(); // User | 

            try
            {
                apiInstance.TestBodyWithQueryParams(query, user);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestBodyWithQueryParams: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestBodyWithQueryParamsWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.TestBodyWithQueryParamsWithHttpInfo(query, user);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestBodyWithQueryParamsWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **query** | **string** |  |  |
| **user** | [**User**](User.md) |  |  |

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
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testclientmodel"></a>
# **TestClientModel**
> ModelClient TestClientModel (ModelClient modelClient)

To test \"client\" model

To test \"client\" model

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestClientModelExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var modelClient = new ModelClient(); // ModelClient | client model

            try
            {
                // To test \"client\" model
                ModelClient result = apiInstance.TestClientModel(modelClient);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestClientModel: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestClientModelWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // To test \"client\" model
    ApiResponse<ModelClient> response = apiInstance.TestClientModelWithHttpInfo(modelClient);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestClientModelWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **modelClient** | [**ModelClient**](ModelClient.md) | client model |  |

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testendpointparameters"></a>
# **TestEndpointParameters**
> void TestEndpointParameters (decimal number, double varDouble, string patternWithoutDelimiter, byte[] varByte, int? integer = null, int? int32 = null, long? int64 = null, float? varFloat = null, string varString = null, System.IO.Stream binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null, string callback = null)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEndpointParametersExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            // Configure HTTP basic authorization: http_basic_test
            config.Username = "YOUR_USERNAME";
            config.Password = "YOUR_PASSWORD";

            var apiInstance = new FakeApi(config);
            var number = 8.14D;  // decimal | None
            var varDouble = 1.2D;  // double | None
            var patternWithoutDelimiter = "patternWithoutDelimiter_example";  // string | None
            var varByte = System.Text.Encoding.ASCII.GetBytes("BYTE_ARRAY_DATA_HERE");  // byte[] | None
            var integer = 56;  // int? | None (optional) 
            var int32 = 56;  // int? | None (optional) 
            var int64 = 789L;  // long? | None (optional) 
            var varFloat = 3.4F;  // float? | None (optional) 
            var varString = "varString_example";  // string | None (optional) 
            var binary = new System.IO.MemoryStream(System.IO.File.ReadAllBytes("/path/to/file.txt"));  // System.IO.Stream | None (optional) 
            var date = DateTime.Parse("2013-10-20");  // DateTime? | None (optional) 
            var dateTime = DateTime.Parse(""2010-02-01T10:20:10.111110+01:00"");  // DateTime? | None (optional)  (default to "2010-02-01T10:20:10.111110+01:00")
            var password = "password_example";  // string | None (optional) 
            var callback = "callback_example";  // string | None (optional) 

            try
            {
                // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
                apiInstance.TestEndpointParameters(number, varDouble, patternWithoutDelimiter, varByte, integer, int32, int64, varFloat, varString, binary, date, dateTime, password, callback);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestEndpointParameters: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEndpointParametersWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    apiInstance.TestEndpointParametersWithHttpInfo(number, varDouble, patternWithoutDelimiter, varByte, integer, int32, int64, varFloat, varString, binary, date, dateTime, password, callback);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestEndpointParametersWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **number** | **decimal** | None |  |
| **varDouble** | **double** | None |  |
| **patternWithoutDelimiter** | **string** | None |  |
| **varByte** | **byte[]** | None |  |
| **integer** | **int?** | None | [optional]  |
| **int32** | **int?** | None | [optional]  |
| **int64** | **long?** | None | [optional]  |
| **varFloat** | **float?** | None | [optional]  |
| **varString** | **string** | None | [optional]  |
| **binary** | **System.IO.Stream****System.IO.Stream** | None | [optional]  |
| **date** | **DateTime?** | None | [optional]  |
| **dateTime** | **DateTime?** | None | [optional] [default to &quot;2010-02-01T10:20:10.111110+01:00&quot;] |
| **password** | **string** | None | [optional]  |
| **callback** | **string** | None | [optional]  |

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testenumparameters"></a>
# **TestEnumParameters**
> void TestEnumParameters (List<string> enumHeaderStringArray = null, string enumHeaderString = null, List<string> enumQueryStringArray = null, string enumQueryString = null, int? enumQueryInteger = null, double? enumQueryDouble = null, List<string> enumFormStringArray = null, string enumFormString = null)

To test enum parameters

To test enum parameters

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEnumParametersExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var enumHeaderStringArray = new List<string>(); // List<string> | Header parameter enum test (string array) (optional) 
            var enumHeaderString = "_abc";  // string | Header parameter enum test (string) (optional)  (default to -efg)
            var enumQueryStringArray = new List<string>(); // List<string> | Query parameter enum test (string array) (optional) 
            var enumQueryString = "_abc";  // string | Query parameter enum test (string) (optional)  (default to -efg)
            var enumQueryInteger = 1;  // int? | Query parameter enum test (double) (optional) 
            var enumQueryDouble = 1.1D;  // double? | Query parameter enum test (double) (optional) 
            var enumFormStringArray = new List<string>(); // List<string> | Form parameter enum test (string array) (optional)  (default to $)
            var enumFormString = "_abc";  // string | Form parameter enum test (string) (optional)  (default to -efg)

            try
            {
                // To test enum parameters
                apiInstance.TestEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestEnumParameters: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEnumParametersWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // To test enum parameters
    apiInstance.TestEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestEnumParametersWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **enumHeaderStringArray** | [**List&lt;string&gt;**](string.md) | Header parameter enum test (string array) | [optional]  |
| **enumHeaderString** | **string** | Header parameter enum test (string) | [optional] [default to -efg] |
| **enumQueryStringArray** | [**List&lt;string&gt;**](string.md) | Query parameter enum test (string array) | [optional]  |
| **enumQueryString** | **string** | Query parameter enum test (string) | [optional] [default to -efg] |
| **enumQueryInteger** | **int?** | Query parameter enum test (double) | [optional]  |
| **enumQueryDouble** | **double?** | Query parameter enum test (double) | [optional]  |
| **enumFormStringArray** | [**List&lt;string&gt;**](string.md) | Form parameter enum test (string array) | [optional] [default to $] |
| **enumFormString** | **string** | Form parameter enum test (string) | [optional] [default to -efg] |

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
| **400** | Invalid request |  -  |
| **404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testgroupparameters"></a>
# **TestGroupParameters**
> void TestGroupParameters (int requiredStringGroup, bool requiredBooleanGroup, long requiredInt64Group, int? stringGroup = null, bool? booleanGroup = null, long? int64Group = null)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestGroupParametersExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            // Configure Bearer token for authorization: bearer_test
            config.AccessToken = "YOUR_BEARER_TOKEN";

            var apiInstance = new FakeApi(config);
            var requiredStringGroup = 56;  // int | Required String in group parameters
            var requiredBooleanGroup = true;  // bool | Required Boolean in group parameters
            var requiredInt64Group = 789L;  // long | Required Integer in group parameters
            var stringGroup = 56;  // int? | String in group parameters (optional) 
            var booleanGroup = true;  // bool? | Boolean in group parameters (optional) 
            var int64Group = 789L;  // long? | Integer in group parameters (optional) 

            try
            {
                // Fake endpoint to test group parameters (optional)
                apiInstance.TestGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestGroupParameters: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestGroupParametersWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Fake endpoint to test group parameters (optional)
    apiInstance.TestGroupParametersWithHttpInfo(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestGroupParametersWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requiredStringGroup** | **int** | Required String in group parameters |  |
| **requiredBooleanGroup** | **bool** | Required Boolean in group parameters |  |
| **requiredInt64Group** | **long** | Required Integer in group parameters |  |
| **stringGroup** | **int?** | String in group parameters | [optional]  |
| **booleanGroup** | **bool?** | Boolean in group parameters | [optional]  |
| **int64Group** | **long?** | Integer in group parameters | [optional]  |

### Return type

void (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Something wrong |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testinlineadditionalproperties"></a>
# **TestInlineAdditionalProperties**
> void TestInlineAdditionalProperties (Dictionary<string, string> requestBody)

test inline additionalProperties

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestInlineAdditionalPropertiesExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var requestBody = new Dictionary<string, string>(); // Dictionary<string, string> | request body

            try
            {
                // test inline additionalProperties
                apiInstance.TestInlineAdditionalProperties(requestBody);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestInlineAdditionalProperties: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestInlineAdditionalPropertiesWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // test inline additionalProperties
    apiInstance.TestInlineAdditionalPropertiesWithHttpInfo(requestBody);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestInlineAdditionalPropertiesWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requestBody** | [**Dictionary&lt;string, string&gt;**](string.md) | request body |  |

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
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testinlinefreeformadditionalproperties"></a>
# **TestInlineFreeformAdditionalProperties**
> void TestInlineFreeformAdditionalProperties (TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest)

test inline free-form additionalProperties

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestInlineFreeformAdditionalPropertiesExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var testInlineFreeformAdditionalPropertiesRequest = new TestInlineFreeformAdditionalPropertiesRequest(); // TestInlineFreeformAdditionalPropertiesRequest | request body

            try
            {
                // test inline free-form additionalProperties
                apiInstance.TestInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestInlineFreeformAdditionalProperties: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestInlineFreeformAdditionalPropertiesWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // test inline free-form additionalProperties
    apiInstance.TestInlineFreeformAdditionalPropertiesWithHttpInfo(testInlineFreeformAdditionalPropertiesRequest);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestInlineFreeformAdditionalPropertiesWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **testInlineFreeformAdditionalPropertiesRequest** | [**TestInlineFreeformAdditionalPropertiesRequest**](TestInlineFreeformAdditionalPropertiesRequest.md) | request body |  |

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
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testjsonformdata"></a>
# **TestJsonFormData**
> void TestJsonFormData (string param, string param2)

test json serialization of form data

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestJsonFormDataExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var param = "param_example";  // string | field1
            var param2 = "param2_example";  // string | field2

            try
            {
                // test json serialization of form data
                apiInstance.TestJsonFormData(param, param2);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestJsonFormData: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestJsonFormDataWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // test json serialization of form data
    apiInstance.TestJsonFormDataWithHttpInfo(param, param2);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestJsonFormDataWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **param** | **string** | field1 |  |
| **param2** | **string** | field2 |  |

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
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testqueryparametercollectionformat"></a>
# **TestQueryParameterCollectionFormat**
> void TestQueryParameterCollectionFormat (List<string> pipe, List<string> ioutil, List<string> http, List<string> url, List<string> context, string requiredNotNullable, string requiredNullable, string notRequiredNotNullable = null, string notRequiredNullable = null)



To test the collection format in query parameters

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryParameterCollectionFormatExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new FakeApi(config);
            var pipe = new List<string>(); // List<string> | 
            var ioutil = new List<string>(); // List<string> | 
            var http = new List<string>(); // List<string> | 
            var url = new List<string>(); // List<string> | 
            var context = new List<string>(); // List<string> | 
            var requiredNotNullable = "requiredNotNullable_example";  // string | 
            var requiredNullable = "requiredNullable_example";  // string | 
            var notRequiredNotNullable = "notRequiredNotNullable_example";  // string |  (optional) 
            var notRequiredNullable = "notRequiredNullable_example";  // string |  (optional) 

            try
            {
                apiInstance.TestQueryParameterCollectionFormat(pipe, ioutil, http, url, context, requiredNotNullable, requiredNullable, notRequiredNotNullable, notRequiredNullable);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FakeApi.TestQueryParameterCollectionFormat: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryParameterCollectionFormatWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    apiInstance.TestQueryParameterCollectionFormatWithHttpInfo(pipe, ioutil, http, url, context, requiredNotNullable, requiredNullable, notRequiredNotNullable, notRequiredNullable);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FakeApi.TestQueryParameterCollectionFormatWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pipe** | [**List&lt;string&gt;**](string.md) |  |  |
| **ioutil** | [**List&lt;string&gt;**](string.md) |  |  |
| **http** | [**List&lt;string&gt;**](string.md) |  |  |
| **url** | [**List&lt;string&gt;**](string.md) |  |  |
| **context** | [**List&lt;string&gt;**](string.md) |  |  |
| **requiredNotNullable** | **string** |  |  |
| **requiredNullable** | **string** |  |  |
| **notRequiredNotNullable** | **string** |  | [optional]  |
| **notRequiredNullable** | **string** |  | [optional]  |

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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

