# \DefaultApi

All URIs are relative to *http://localhost:8080*

Method | HTTP request | Description
------------- | ------------- | -------------
[**list**](DefaultApi.md#list) | **GET** /pony | 



## list

> list(page_query, page_query_schema, deep, not_required, not_required_deep)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**page_query** | [**ListPageQueryParameter**](.md) |  | [required] |
**page_query_schema** | [**Page**](.md) |  | [required] |
**deep** | [**ListDeepParameter**](.md) |  | [required] |
**not_required** | Option<[**ListNotRequiredParameter**](.md)> |  |  |
**not_required_deep** | Option<[**ListNotRequiredDeepParameter**](.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

