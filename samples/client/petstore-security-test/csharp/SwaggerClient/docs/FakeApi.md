# IO.Swagger.Api.FakeApi

All URIs are relative to *https://petstore.swagger.io  ' \" =end/v2  ' \" =end*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestCodeInjectEnd**](FakeApi.md#testcodeinjectend) | **PUT** /fake | To test code injection  &#39; \&quot; &#x3D;end


# **TestCodeInjectEnd**
> void TestCodeInjectEnd (string testCodeInjectEnd = null)

To test code injection  ' \" =end

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace Example
{
    public class TestCodeInjectEndExample
    {
        public void main()
        {
            
            var apiInstance = new FakeApi();
            var testCodeInjectEnd = testCodeInjectEnd_example;  // string | To test code injection  ' \" =end (optional) 

            try
            {
                // To test code injection  ' \" =end
                apiInstance.TestCodeInjectEnd(testCodeInjectEnd);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling FakeApi.TestCodeInjectEnd: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **testCodeInjectEnd** | **string**| To test code injection  &#39; \&quot; &#x3D;end | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */ '  =end
 - **Accept**: application/json, */ '  =end

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

