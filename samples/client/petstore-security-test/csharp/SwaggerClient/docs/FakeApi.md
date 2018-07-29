# Org.OpenAPITools.Api.FakeApi

All URIs are relative to *petstore.swagger.io *_/ ' \" =end - - \\r\\n \\n \\r/v2 *_/ ' \" =end - - \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestCodeInjectEndRnNR**](FakeApi.md#testcodeinjectendrnnr) | **PUT** /fake | To test code injection *_/ &#39; \&quot; &#x3D;end - - \\r\\n \\n \\r


<a name="testcodeinjectendrnnr"></a>
# **TestCodeInjectEndRnNR**
> void TestCodeInjectEndRnNR (Object UNKNOWN_BASE_TYPE = null)

To test code injection *_/ ' \" =end - - \\r\\n \\n \\r

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestCodeInjectEndRnNRExample
    {
        public void main()
        {
            var apiInstance = new FakeApi();
            var UNKNOWN_BASE_TYPE = new Object(); // Object |  (optional) 

            try
            {
                // To test code injection *_/ ' \" =end - - \\r\\n \\n \\r
                apiInstance.TestCodeInjectEndRnNR(UNKNOWN_BASE_TYPE);
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
 **UNKNOWN_BASE_TYPE** | [**Object**](Object.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, *_/ '  =end - -       
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

