# \FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_parameter_name_mapping**](FakeApi.md#get_parameter_name_mapping) | **GET** /fake/parameter-name-mapping | parameter name mapping test



## get_parameter_name_mapping

> get_parameter_name_mapping(underscore_type, r#type, type_with_underscore, dash_type, http_debug_option)
parameter name mapping test

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**underscore_type** | **i64** | _type | [required] |
**r#type** | **String** | type | [required] |
**type_with_underscore** | **String** | type_ | [required] |
**dash_type** | **String** | -type | [required] |
**http_debug_option** | **String** | http debug option (to test parameter naming option) | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

