# default_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
****](default_api.md#) | **POST** /multipart_related_request | 
****](default_api.md#) | **POST** /multipart_request | 
****](default_api.md#) | **POST** /multiple-identical-mime-types | 


# ****
> (required_binary_field, optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **required_binary_field** | **swagger::ByteArray**|  | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **required_binary_field** | **swagger::ByteArray**|  | 
 **object_field** | [**multipart_request_object_field**](multipart_request_object_field.md)|  | 
 **optional_binary_field** | **swagger::ByteArray**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/related
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# ****
> (string_field, binary_field, optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **string_field** | **String**|  | 
  **binary_field** | **swagger::ByteArray**|  | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **string_field** | **String**|  | 
 **binary_field** | **swagger::ByteArray**|  | 
 **optional_string_field** | **String**|  | 
 **object_field** | [**multipart_request_object_field**](multipart_request_object_field.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# ****
> (optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **binary1** | **swagger::ByteArray**|  | 
 **binary2** | **swagger::ByteArray**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/related
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

