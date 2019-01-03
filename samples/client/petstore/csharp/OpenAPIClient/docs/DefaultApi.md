# Org.OpenAPITools.Api.DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateXmlItem**](DefaultApi.md#createxmlitem) | **POST** /fake/create_xml_item | creates an XmlItem


<a name="createxmlitem"></a>
# **CreateXmlItem**
> void CreateXmlItem (XmlItem xmlItem)

creates an XmlItem

this route creates an XmlItem

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class CreateXmlItemExample
    {
        public void main()
        {
            var apiInstance = new DefaultApi();
            var xmlItem = new XmlItem(); // XmlItem | XmlItem Body

            try
            {
                // creates an XmlItem
                apiInstance.CreateXmlItem(xmlItem);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling DefaultApi.CreateXmlItem: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

