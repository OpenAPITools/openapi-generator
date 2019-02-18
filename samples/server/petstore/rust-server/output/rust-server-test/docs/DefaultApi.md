# \default

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Default**](defaultapi.md#Default) | **GET** /dummy | A dummy endpoint to make the spec valid.
[**Default**](defaultapi.md#Default) | **PUT** /dummy | 
[**Default**](defaultapi.md#Default) | **GET** /file_response | Get a file
[**Default**](defaultapi.md#Default) | **POST** /html | Test HTML handling
[**Default**](defaultapi.md#Default) | **GET** /raw_json | Get an arbitrary JSON blob.


# **DummyGet**
> DummyGet()
A dummy endpoint to make the spec valid.

### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **DummyPut**
> DummyPut(nested_response)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **nested_response** | [**InlineObject**](InlineObject.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FileResponseGet**
> swagger::ByteArray FileResponseGet()
Get a file

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**swagger::ByteArray**](file.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **HtmlPost**
> String HtmlPost(body)
Test HTML handling

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | **String**|  | 

### Return type

[**String**](string.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: text/html
 - **Accept**: text/html, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **RawJsonGet**
> object RawJsonGet()
Get an arbitrary JSON blob.

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**object**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

