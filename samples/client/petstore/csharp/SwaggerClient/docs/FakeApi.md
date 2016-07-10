# IO.Swagger.Api.FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumQueryParameters**](FakeApi.md#testenumqueryparameters) | **GET** /fake | To test enum query parameters


<a name="testclientmodel"></a>
# **TestClientModel**
> ModelClient TestClientModel (ModelClient body)

To test \"client\" model

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestClientModelExample
    {
        public void main()
        {
            
            var apiInstance = new FakeApi();
            var body = new ModelClient(); // ModelClient | client model

            try
            {
                // To test \"client\" model
                ModelClient result = apiInstance.TestClientModel(body);
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
 **body** | [**ModelClient**](ModelClient.md)| client model | 

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
> void TestEndpointParameters (decimal? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestEndpointParametersExample
    {
        public void main()
        {
            
            var apiInstance = new FakeApi();
            var number = 3.4;  // decimal? | None
            var _double = 1.2;  // double? | None
            var _string = _string_example;  // string | None
            var _byte = B;  // byte[] | None
            var integer = 56;  // int? | None (optional) 
            var int32 = 56;  // int? | None (optional) 
            var int64 = 789;  // long? | None (optional) 
            var _float = 3.4;  // float? | None (optional) 
            var binary = B;  // byte[] | None (optional) 
            var date = 2013-10-20;  // DateTime? | None (optional) 
            var dateTime = 2013-10-20T19:20:30+01:00;  // DateTime? | None (optional) 
            var password = password_example;  // string | None (optional) 

            try
            {
                // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
                apiInstance.TestEndpointParameters(number, _double, _string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);
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
 **_string** | **string**| None | 
 **_byte** | **byte[]**| None | 
 **integer** | **int?**| None | [optional] 
 **int32** | **int?**| None | [optional] 
 **int64** | **long?**| None | [optional] 
 **_float** | **float?**| None | [optional] 
 **binary** | **byte[]**| None | [optional] 
 **date** | **DateTime?**| None | [optional] 
 **dateTime** | **DateTime?**| None | [optional] 
 **password** | **string**| None | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="testenumqueryparameters"></a>
# **TestEnumQueryParameters**
> void TestEnumQueryParameters (string enumQueryString = null, decimal? enumQueryInteger = null, double? enumQueryDouble = null)

To test enum query parameters

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestEnumQueryParametersExample
    {
        public void main()
        {
            
            var apiInstance = new FakeApi();
            var enumQueryString = enumQueryString_example;  // string | Query parameter enum test (string) (optional)  (default to -efg)
            var enumQueryInteger = 3.4;  // decimal? | Query parameter enum test (double) (optional) 
            var enumQueryDouble = 1.2;  // double? | Query parameter enum test (double) (optional) 

            try
            {
                // To test enum query parameters
                apiInstance.TestEnumQueryParameters(enumQueryString, enumQueryInteger, enumQueryDouble);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestEnumQueryParameters: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumQueryString** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **decimal?**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **double?**| Query parameter enum test (double) | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

