# Org.OpenAPITools.Api.FormApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**TestFormIntegerBooleanString**](FormApi.md#testformintegerbooleanstring) | **POST** /form/integer/boolean/string | Test form parameter(s) |
| [**TestFormOneof**](FormApi.md#testformoneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema |

<a id="testformintegerbooleanstring"></a>
# **TestFormIntegerBooleanString**
> string TestFormIntegerBooleanString (int? integerForm = null, bool? booleanForm = null, string? stringForm = null)

Test form parameter(s)

Test form parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestFormIntegerBooleanStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new FormApi(config);
            var integerForm = 56;  // int? |  (optional) 
            var booleanForm = true;  // bool? |  (optional) 
            var stringForm = "stringForm_example";  // string? |  (optional) 

            try
            {
                // Test form parameter(s)
                string result = apiInstance.TestFormIntegerBooleanString(integerForm, booleanForm, stringForm);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FormApi.TestFormIntegerBooleanString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestFormIntegerBooleanStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test form parameter(s)
    ApiResponse<string> response = apiInstance.TestFormIntegerBooleanStringWithHttpInfo(integerForm, booleanForm, stringForm);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FormApi.TestFormIntegerBooleanStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **integerForm** | **int?** |  | [optional]  |
| **booleanForm** | **bool?** |  | [optional]  |
| **stringForm** | **string?** |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testformoneof"></a>
# **TestFormOneof**
> string TestFormOneof (string? form1 = null, int? form2 = null, string? form3 = null, bool? form4 = null, long? id = null, string? name = null)

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestFormOneofExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new FormApi(config);
            var form1 = "form1_example";  // string? |  (optional) 
            var form2 = 56;  // int? |  (optional) 
            var form3 = "form3_example";  // string? |  (optional) 
            var form4 = true;  // bool? |  (optional) 
            var id = 789L;  // long? |  (optional) 
            var name = "name_example";  // string? |  (optional) 

            try
            {
                // Test form parameter(s) for oneOf schema
                string result = apiInstance.TestFormOneof(form1, form2, form3, form4, id, name);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling FormApi.TestFormOneof: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestFormOneofWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test form parameter(s) for oneOf schema
    ApiResponse<string> response = apiInstance.TestFormOneofWithHttpInfo(form1, form2, form3, form4, id, name);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling FormApi.TestFormOneofWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **form1** | **string?** |  | [optional]  |
| **form2** | **int?** |  | [optional]  |
| **form3** | **string?** |  | [optional]  |
| **form4** | **bool?** |  | [optional]  |
| **id** | **long?** |  | [optional]  |
| **name** | **string?** |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

