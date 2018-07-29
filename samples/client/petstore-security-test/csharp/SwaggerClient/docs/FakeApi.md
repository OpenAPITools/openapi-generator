# IO.Swagger.Api.FakeApi

All URIs are relative to *https://petstore.swagger.io *_/ ' \" =end - - \\r\\n \\n \\r/v2 *_/ ' \" =end - - \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestCodeInjectEndRnNR**](FakeApi.md#testcodeinjectendrnnr) | **PUT** /fake | To test code injection *_/ &#39; \&quot; &#x3D;end - - \\r\\n \\n \\r


<a name="testcodeinjectendrnnr"></a>
# **TestCodeInjectEndRnNR**
> void TestCodeInjectEndRnNR (string testCodeInjectEndRnNR = null)

To test code injection *_/ ' \" =end - - \\r\\n \\n \\r

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestCodeInjectEndRnNRExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var testCodeInjectEndRnNR = testCodeInjectEndRnNR_example;  // string | To test code injection *_/ ' \" =end - - \\r\\n \\n \\r (optional) 

            try
            {
                // To test code injection *_/ ' \" =end - - \\r\\n \\n \\r
                apiInstance.TestCodeInjectEndRnNR(testCodeInjectEndRnNR);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestCodeInjectEndRnNR: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **testCodeInjectEndRnNR** | **string**| To test code injection *_/ &#39; \&quot; &#x3D;end - - \\r\\n \\n \\r | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, *_/ '  =end - -       
 - **Accept**: application/json, *_/ '  =end - -       

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

