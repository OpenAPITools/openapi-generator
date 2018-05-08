# Org.OpenAPITools.Api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeOuterBooleanSerialize**](FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string | 
[**TestBodyWithQueryParams**](FakeApi.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params | 
[**TestClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumParameters**](FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters
[**TestInlineAdditionalProperties**](FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**TestJsonFormData**](FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data


<a name="fakeouterbooleanserialize"></a>
# **FakeOuterBooleanSerialize**
> bool? FakeOuterBooleanSerialize (bool? body = null)



Test serialization of outer boolean types

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterBooleanSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = true;  // bool? | Input boolean as post body (optional) 

            try
            {
                bool? result = apiInstance.FakeOuterBooleanSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterBooleanSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bool?**| Input boolean as post body | [optional] 

### Return type

**bool?**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="fakeoutercompositeserialize"></a>
# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize (OuterComposite outerComposite = null)



Test serialization of object with outer number type

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterCompositeSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var outerComposite = new OuterComposite(); // OuterComposite | Input composite as post body (optional) 

            try
            {
                OuterComposite result = apiInstance.FakeOuterCompositeSerialize(outerComposite);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterCompositeSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="fakeouternumberserialize"></a>
# **FakeOuterNumberSerialize**
> decimal? FakeOuterNumberSerialize (decimal? body = null)



Test serialization of outer number types

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterNumberSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = 1.2;  // decimal? | Input number as post body (optional) 

            try
            {
                decimal? result = apiInstance.FakeOuterNumberSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterNumberSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **decimal?**| Input number as post body | [optional] 

### Return type

**decimal?**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="fakeouterstringserialize"></a>
# **FakeOuterStringSerialize**
> string FakeOuterStringSerialize (string body = null)



Test serialization of outer string types

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FakeOuterStringSerializeExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var body = body_example;  // string | Input string as post body (optional) 

            try
            {
                string result = apiInstance.FakeOuterStringSerialize(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.FakeOuterStringSerialize: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**| Input string as post body | [optional] 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testbodywithqueryparams"></a>
# **TestBodyWithQueryParams**
> void TestBodyWithQueryParams (string query, User user)



### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestBodyWithQueryParamsExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var query = query_example;  // string | 
            var user = new User(); // User | 

            try
            {
                apiInstance.TestBodyWithQueryParams(query, user);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestBodyWithQueryParams: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **string**|  | 
 **user** | [**User**](User.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testclientmodel"></a>
# **TestClientModel**
> ModelClient TestClientModel (ModelClient modelClient)

To test \"client\" model

To test \"client\" model

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestClientModelExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var modelClient = new ModelClient(); // ModelClient | client model

            try
            {
                // To test \"client\" model
                ModelClient result = apiInstance.TestClientModel(modelClient);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestClientModel: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **modelClient** | [**ModelClient**](ModelClient.md)| client model | 

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testendpointparameters"></a>
# **TestEndpointParameters**
> void TestEndpointParameters (decimal? number, double? _double, string patternWithoutDelimiter, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, string _string = null, System.IO.Stream binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null, string callback = null)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEndpointParametersExample
    {
        public void main()
        {
            // Configure HTTP basic authorization: http_basic_test
            Configuration.Default.Username = "YOUR_USERNAME";
            Configuration.Default.Password = "YOUR_PASSWORD";

            var apiInstance = new FakeApi();
            var number = 8.14;  // decimal? | None
            var _double = 1.2;  // double? | None
            var patternWithoutDelimiter = patternWithoutDelimiter_example;  // string | None
            var _byte = BYTE_ARRAY_DATA_HERE;  // byte[] | None
            var integer = 56;  // int? | None (optional) 
            var int32 = 56;  // int? | None (optional) 
            var int64 = 789;  // long? | None (optional) 
            var _float = 3.4;  // float? | None (optional) 
            var _string = _string_example;  // string | None (optional) 
            var binary = BINARY_DATA_HERE;  // System.IO.Stream | None (optional) 
            var date = 2013-10-20;  // DateTime? | None (optional) 
            var dateTime = 2013-10-20T19:20:30+01:00;  // DateTime? | None (optional) 
            var password = password_example;  // string | None (optional) 
            var callback = callback_example;  // string | None (optional) 

            try
            {
                // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
                apiInstance.TestEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, _string, binary, date, dateTime, password, callback);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestEndpointParameters: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **decimal?**| None | 
 **_double** | **double?**| None | 
 **patternWithoutDelimiter** | **string**| None | 
 **_byte** | **byte[]**| None | 
 **integer** | **int?**| None | [optional] 
 **int32** | **int?**| None | [optional] 
 **int64** | **long?**| None | [optional] 
 **_float** | **float?**| None | [optional] 
 **_string** | **string**| None | [optional] 
 **binary** | **System.IO.Stream**| None | [optional] 
 **date** | **DateTime?**| None | [optional] 
 **dateTime** | **DateTime?**| None | [optional] 
 **password** | **string**| None | [optional] 
 **callback** | **string**| None | [optional] 

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testenumparameters"></a>
# **TestEnumParameters**
> void TestEnumParameters (List<string> enumHeaderStringArray = null, string enumHeaderString = null, List<string> enumQueryStringArray = null, string enumQueryString = null, int? enumQueryInteger = null, double? enumQueryDouble = null, List<string> enumFormStringArray = null, string enumFormString = null)

To test enum parameters

To test enum parameters

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestEnumParametersExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var enumHeaderStringArray = enumHeaderStringArray_example;  // List<string> | Header parameter enum test (string array) (optional) 
            var enumHeaderString = enumHeaderString_example;  // string | Header parameter enum test (string) (optional)  (default to -efg)
            var enumQueryStringArray = enumQueryStringArray_example;  // List<string> | Query parameter enum test (string array) (optional) 
            var enumQueryString = enumQueryString_example;  // string | Query parameter enum test (string) (optional)  (default to -efg)
            var enumQueryInteger = 56;  // int? | Query parameter enum test (double) (optional) 
            var enumQueryDouble = 1.2;  // double? | Query parameter enum test (double) (optional) 
            var enumFormStringArray = enumFormStringArray_example;  // List<string> | Form parameter enum test (string array) (optional)  (default to $)
            var enumFormString = enumFormString_example;  // string | Form parameter enum test (string) (optional)  (default to -efg)

            try
            {
                // To test enum parameters
                apiInstance.TestEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestEnumParameters: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | **List&lt;string&gt;**| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **string**| Header parameter enum test (string) | [optional] [default to -efg]
 **enumQueryStringArray** | **List&lt;string&gt;**| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **int?**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **double?**| Query parameter enum test (double) | [optional] 
 **enumFormStringArray** | **List&lt;string&gt;**| Form parameter enum test (string array) | [optional] [default to $]
 **enumFormString** | **string**| Form parameter enum test (string) | [optional] [default to -efg]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testinlineadditionalproperties"></a>
# **TestInlineAdditionalProperties**
> void TestInlineAdditionalProperties (Dictionary<string, string> requestBody)

test inline additionalProperties

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestInlineAdditionalPropertiesExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var requestBody = new Dictionary<string, string>(); // Dictionary<string, string> | request body

            try
            {
                // test inline additionalProperties
                apiInstance.TestInlineAdditionalProperties(requestBody);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestInlineAdditionalProperties: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**Dictionary&lt;string, string&gt;**](string.md)| request body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testjsonformdata"></a>
# **TestJsonFormData**
> void TestJsonFormData (string param, string param2)

test json serialization of form data

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestJsonFormDataExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var param = param_example;  // string | field1
            var param2 = param2_example;  // string | field2

            try
            {
                // test json serialization of form data
                apiInstance.TestJsonFormData(param, param2);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestJsonFormData: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string**| field1 | 
 **param2** | **string**| field2 | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

