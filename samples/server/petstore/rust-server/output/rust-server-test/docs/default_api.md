# default_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
**AllOf_Get**](default_api.md#AllOf_Get) | **GET** /allOf | 
**dummyGet**](default_api.md#dummyGet) | **GET** /dummy | A dummy endpoint to make the spec valid.
**dummyPut**](default_api.md#dummyPut) | **PUT** /dummy | 
**file_responseGet**](default_api.md#file_responseGet) | **GET** /file_response | Get a file
**getStructuredYaml**](default_api.md#getStructuredYaml) | **GET** /get-structured-yaml | 
**htmlPost**](default_api.md#htmlPost) | **POST** /html | Test HTML handling
**post_yaml**](default_api.md#post_yaml) | **POST** /post-yaml | 
**raw_jsonGet**](default_api.md#raw_jsonGet) | **GET** /raw_json | Get an arbitrary JSON blob.
**solo_objectPost**](default_api.md#solo_objectPost) | **POST** /solo-object | Send an arbitrary JSON blob


# **AllOf_Get**
> models::AllOfObject AllOf_Get()


Test getting an object which uses allOf

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**models::AllOfObject**](allOfObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **dummyGet**
> dummyGet()
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

# **dummyPut**
> dummyPut(nested_response)


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

# **file_responseGet**
> swagger::ByteArray file_responseGet()
Get a file

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**swagger::ByteArray**](file.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

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
 - **Accept**: application/yaml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **htmlPost**
> String htmlPost(body)
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
 - **Accept**: text/html

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

# **raw_jsonGet**
> serde_json::Value raw_jsonGet()
Get an arbitrary JSON blob.

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**serde_json::Value**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **solo_objectPost**
> solo_objectPost(value)
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

