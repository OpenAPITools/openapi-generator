# \DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_widget_summary**](DefaultApi.md#get_widget_summary) | **GET** /widgets/summary | Get a widget summary
[**get_widget_summary_by_id**](DefaultApi.md#get_widget_summary_by_id) | **GET** /widgets/{widget_id}/summary | Get a widget summary for a single widget



## get_widget_summary

> models::WidgetSummary get_widget_summary(kind, sort)
Get a widget summary

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**kind** | Option<**String**> | Kind filter, expressed as an anyOf with null |  |
**sort** | Option<**String**> | Sort order, expressed as a $ref with sibling keywords |  |

### Return type

[**models::WidgetSummary**](WidgetSummary.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_widget_summary_by_id

> models::WidgetSummary get_widget_summary_by_id(widget_id, kind)
Get a widget summary for a single widget

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**widget_id** | **String** |  | [required] |
**kind** | Option<**String**> | Kind filter, expressed as an anyOf with null |  |

### Return type

[**models::WidgetSummary**](WidgetSummary.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

