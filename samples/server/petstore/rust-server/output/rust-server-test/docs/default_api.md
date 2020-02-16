# default_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
****](default_api.md#) | **GET** /dummy | A dummy endpoint to make the spec valid.
****](default_api.md#) | **PUT** /dummy | 
****](default_api.md#) | **GET** /file_response | Get a file
**getStructuredYaml**](default_api.md#getStructuredYaml) | **GET** /get-structured-yaml | 
****](default_api.md#) | **POST** /html | Test HTML handling
**post_yaml**](default_api.md#post_yaml) | **POST** /post-yaml | 
****](default_api.md#) | **GET** /raw_json | Get an arbitrary JSON blob.
****](default_api.md#) | **POST** /solo-object | Send an arbitrary JSON blob


# ****
> ()
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

# ****
> (nested_response)


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

# ****
> swagger::ByteArray ()
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

# **getStructuredYaml**
> models::GetYamlResponse getStructuredYaml()


Test returning arbitrary structured YAML

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**models::GetYamlResponse**](get_yaml_response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/yaml, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# ****
> String (body)
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

# **post_yaml**
> post_yaml(value)


Test sending an arbitrary unsupported format - e.g. YAML

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **value** | **String**| The YAML body to test | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/yaml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# ****
> serde_json::Value ()
Get an arbitrary JSON blob.

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**serde_json::Value**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# ****
> (value)
Send an arbitrary JSON blob

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **value** | **serde_json::Value**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

