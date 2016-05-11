# IO.Swagger.Api.FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters


# **TestEndpointParameters**
> void TestEndpointParameters (double? number, double? _double, string _string, byte[] _byte, int? integer = null, int? int32 = null, long? int64 = null, float? _float = null, byte[] binary = null, DateTime? date = null, DateTime? dateTime = null, string password = null)

Fake endpoint for testing various parameters

Fake endpoint for testing various parameters

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
            var number = 3.4;  // double? | None
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
                // Fake endpoint for testing various parameters
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
 **number** | **double?**| None | 
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

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

