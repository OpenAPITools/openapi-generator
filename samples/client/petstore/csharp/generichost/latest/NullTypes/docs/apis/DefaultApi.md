# Org.OpenAPITools.Api.DefaultApi

All URIs are relative to *http://api.example.com/v1*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**GetWidget**](DefaultApi.md#getwidget) | **GET** /widgets/{id} | Retrieve a widget |

<a id="getwidget"></a>
# **GetWidget**
> Widget GetWidget (long id)

Retrieve a widget


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **id** | **long** |  |  |

### Return type

[**Widget**](Widget.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | A widget |  -  |
| **404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

