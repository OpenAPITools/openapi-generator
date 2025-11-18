# \DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_aggregate_data**](DefaultApi.md#get_aggregate_data) | **GET** /aggregate | Get aggregated data
[**get_items**](DefaultApi.md#get_items) | **GET** /items | Get items with filters



## get_aggregate_data

> models::AggregateResponse get_aggregate_data(status, time_bucket, sort_direction)
Get aggregated data

Test endpoint with enum query parameters referenced via $ref

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**status** | [**Status**](.md) | Status filter | [required] |
**time_bucket** | Option<[**TimeBucket**](.md)> | Time aggregation bucket |  |
**sort_direction** | Option<[**SortDirection**](.md)> | Sort direction |  |

### Return type

[**models::AggregateResponse**](AggregateResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_items

> Vec<models::Item> get_items(category, priority)
Get items with filters

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**category** | Option<**String**> | Item category (inline enum) |  |
**priority** | Option<[**Priority**](.md)> | Priority level (enum via ref) |  |

### Return type

[**Vec<models::Item>**](Item.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

