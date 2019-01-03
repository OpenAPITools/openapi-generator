# DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createXmlItem**](DefaultApi.md#createXmlItem) | **POST** fake/create_xml_item | creates an XmlItem


<a name="createXmlItem"></a>
# **createXmlItem**
> createXmlItem(xmlItem)

creates an XmlItem

this route creates an XmlItem

### Example
```java
// Import classes:
//import org.openapitools.client.ApiException;
//import org.openapitools.client.api.DefaultApi;


DefaultApi apiInstance = new DefaultApi();
XmlItem xmlItem = new XmlItem(); // XmlItem | XmlItem Body
try {
    apiInstance.createXmlItem(xmlItem);
} catch (ApiException e) {
    System.err.println("Exception when calling DefaultApi#createXmlItem");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

