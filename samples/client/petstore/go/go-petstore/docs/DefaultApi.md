# \DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateXmlItem**](DefaultApi.md#CreateXmlItem) | **Post** /fake/create_xml_item | creates an XmlItem


# **CreateXmlItem**
> CreateXmlItem(ctx, xmlItem)
creates an XmlItem

this route creates an XmlItem

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

