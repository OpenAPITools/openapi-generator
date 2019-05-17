# Org.OpenAPITools.Api.AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestSpecialTags**](AnotherFakeApi.md#testspecialtags) | **PATCH** /another-fake/dummy | To test special tags


<a name="testspecialtags"></a>
# **TestSpecialTags**
> ModelClient TestSpecialTags (ModelClient modelClient)

To test special tags

To test special tags

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class TestSpecialTagsExample
    {
        public void main()
        {
            var apiInstance = new AnotherFakeApi();
            var modelClient = new ModelClient(); // ModelClient | client model

            try
            {
                // To test special tags
                ModelClient result = apiInstance.TestSpecialTags(modelClient);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling AnotherFakeApi.TestSpecialTags: " + e.Message );
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

