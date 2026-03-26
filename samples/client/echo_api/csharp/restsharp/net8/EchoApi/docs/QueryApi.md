# Org.OpenAPITools.Api.QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**TestEnumRefString**](QueryApi.md#testenumrefstring) | **GET** /query/enum_ref_string | Test query parameter(s) |
| [**TestQueryDatetimeDateString**](QueryApi.md#testquerydatetimedatestring) | **GET** /query/datetime/date/string | Test query parameter(s) |
| [**TestQueryIntegerBooleanString**](QueryApi.md#testqueryintegerbooleanstring) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**TestQueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#testquerystyledeepobjectexplodetrueobject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**TestQueryStyleDeepObjectExplodeTrueObjectAllOf**](QueryApi.md#testquerystyledeepobjectexplodetrueobjectallof) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s) |
| [**TestQueryStyleFormExplodeFalseArrayInteger**](QueryApi.md#testquerystyleformexplodefalsearrayinteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s) |
| [**TestQueryStyleFormExplodeFalseArrayString**](QueryApi.md#testquerystyleformexplodefalsearraystring) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s) |
| [**TestQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testquerystyleformexplodetruearraystring) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**TestQueryStyleFormExplodeTrueObject**](QueryApi.md#testquerystyleformexplodetrueobject) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |
| [**TestQueryStyleFormExplodeTrueObjectAllOf**](QueryApi.md#testquerystyleformexplodetrueobjectallof) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s) |
| [**TestQueryStyleJsonSerializationObject**](QueryApi.md#testquerystylejsonserializationobject) | **GET** /query/style_jsonSerialization/object | Test query parameter(s) |

<a id="testenumrefstring"></a>
# **TestEnumRefString**
> string TestEnumRefString (string? enumNonrefStringQuery = null, StringEnumRef? enumRefStringQuery = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEnumRefStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var enumNonrefStringQuery = "success";  // string? |  (optional) 
            var enumRefStringQuery = new StringEnumRef?(); // StringEnumRef? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestEnumRefString(enumNonrefStringQuery, enumRefStringQuery);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestEnumRefString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestEnumRefStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestEnumRefStringWithHttpInfo(enumNonrefStringQuery, enumRefStringQuery);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestEnumRefStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **enumNonrefStringQuery** | **string?** |  | [optional]  |
| **enumRefStringQuery** | [**StringEnumRef?**](StringEnumRef?.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerydatetimedatestring"></a>
# **TestQueryDatetimeDateString**
> string TestQueryDatetimeDateString (DateTime? datetimeQuery = null, DateOnly? dateQuery = null, string? stringQuery = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryDatetimeDateStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var datetimeQuery = DateTime.Parse("2013-10-20T19:20:30+01:00");  // DateTime? |  (optional) 
            var dateQuery = DateOnly.Parse("2013-10-20");  // DateOnly? |  (optional) 
            var stringQuery = "stringQuery_example";  // string? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryDatetimeDateString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryDatetimeDateStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryDatetimeDateStringWithHttpInfo(datetimeQuery, dateQuery, stringQuery);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryDatetimeDateStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **datetimeQuery** | **DateTime?** |  | [optional]  |
| **dateQuery** | **DateOnly?** |  | [optional]  |
| **stringQuery** | **string?** |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testqueryintegerbooleanstring"></a>
# **TestQueryIntegerBooleanString**
> string TestQueryIntegerBooleanString (int? integerQuery = null, bool? booleanQuery = null, string? stringQuery = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryIntegerBooleanStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var integerQuery = 56;  // int? |  (optional) 
            var booleanQuery = true;  // bool? |  (optional) 
            var stringQuery = "stringQuery_example";  // string? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryIntegerBooleanString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryIntegerBooleanStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryIntegerBooleanStringWithHttpInfo(integerQuery, booleanQuery, stringQuery);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryIntegerBooleanStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **integerQuery** | **int?** |  | [optional]  |
| **booleanQuery** | **bool?** |  | [optional]  |
| **stringQuery** | **string?** |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyledeepobjectexplodetrueobject"></a>
# **TestQueryStyleDeepObjectExplodeTrueObject**
> string TestQueryStyleDeepObjectExplodeTrueObject (Pet? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleDeepObjectExplodeTrueObjectExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new Pet?(); // Pet? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleDeepObjectExplodeTrueObject(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleDeepObjectExplodeTrueObject: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**Pet?**](Pet?.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyledeepobjectexplodetrueobjectallof"></a>
# **TestQueryStyleDeepObjectExplodeTrueObjectAllOf**
> string TestQueryStyleDeepObjectExplodeTrueObjectAllOf (TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleDeepObjectExplodeTrueObjectAllOfExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter?(); // TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectAllOf: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter?**](TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter?.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyleformexplodefalsearrayinteger"></a>
# **TestQueryStyleFormExplodeFalseArrayInteger**
> string TestQueryStyleFormExplodeFalseArrayInteger (List<int>? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleFormExplodeFalseArrayIntegerExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new List<int>?(); // List<int>? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleFormExplodeFalseArrayInteger(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeFalseArrayInteger: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**List&lt;int&gt;?**](int.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyleformexplodefalsearraystring"></a>
# **TestQueryStyleFormExplodeFalseArrayString**
> string TestQueryStyleFormExplodeFalseArrayString (List<string>? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleFormExplodeFalseArrayStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new List<string>?(); // List<string>? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleFormExplodeFalseArrayString(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeFalseArrayString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleFormExplodeFalseArrayStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleFormExplodeFalseArrayStringWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeFalseArrayStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**List&lt;string&gt;?**](string.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyleformexplodetruearraystring"></a>
# **TestQueryStyleFormExplodeTrueArrayString**
> string TestQueryStyleFormExplodeTrueArrayString (TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleFormExplodeTrueArrayStringExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter?(); // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleFormExplodeTrueArrayString(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeTrueArrayString: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleFormExplodeTrueArrayStringWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeTrueArrayStringWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter?**](TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter?.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyleformexplodetrueobject"></a>
# **TestQueryStyleFormExplodeTrueObject**
> string TestQueryStyleFormExplodeTrueObject (Pet? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleFormExplodeTrueObjectExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new Pet?(); // Pet? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleFormExplodeTrueObject(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeTrueObject: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleFormExplodeTrueObjectWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeTrueObjectWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**Pet?**](Pet?.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystyleformexplodetrueobjectallof"></a>
# **TestQueryStyleFormExplodeTrueObjectAllOf**
> string TestQueryStyleFormExplodeTrueObjectAllOf (DataQuery? queryObject = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleFormExplodeTrueObjectAllOfExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var queryObject = new DataQuery?(); // DataQuery? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleFormExplodeTrueObjectAllOf(queryObject);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeTrueObjectAllOf: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **queryObject** | [**DataQuery?**](DataQuery?.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="testquerystylejsonserializationobject"></a>
# **TestQueryStyleJsonSerializationObject**
> string TestQueryStyleJsonSerializationObject (Pet? jsonSerializedObjectRefStringQuery = null, List<Pet>? jsonSerializedObjectArrayRefStringQuery = null)

Test query parameter(s)

Test query parameter(s)

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestQueryStyleJsonSerializationObjectExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://localhost:3000";
            var apiInstance = new QueryApi(config);
            var jsonSerializedObjectRefStringQuery = new Pet?(); // Pet? |  (optional) 
            var jsonSerializedObjectArrayRefStringQuery = new List<Pet>?(); // List<Pet>? |  (optional) 

            try
            {
                // Test query parameter(s)
                string result = apiInstance.TestQueryStyleJsonSerializationObject(jsonSerializedObjectRefStringQuery, jsonSerializedObjectArrayRefStringQuery);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling QueryApi.TestQueryStyleJsonSerializationObject: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the TestQueryStyleJsonSerializationObjectWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Test query parameter(s)
    ApiResponse<string> response = apiInstance.TestQueryStyleJsonSerializationObjectWithHttpInfo(jsonSerializedObjectRefStringQuery, jsonSerializedObjectArrayRefStringQuery);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling QueryApi.TestQueryStyleJsonSerializationObjectWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **jsonSerializedObjectRefStringQuery** | [**Pet?**](Pet?.md) |  | [optional]  |
| **jsonSerializedObjectArrayRefStringQuery** | [**List&lt;Pet&gt;?**](Pet.md) |  | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

