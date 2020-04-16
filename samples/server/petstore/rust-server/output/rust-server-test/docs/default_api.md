# default_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
**AllOfGet**](default_api.md#AllOfGet) | **GET** /allOf | 
**DummyGet**](default_api.md#DummyGet) | **GET** /dummy | A dummy endpoint to make the spec valid.
**DummyPut**](default_api.md#DummyPut) | **PUT** /dummy | 
**FileResponseGet**](default_api.md#FileResponseGet) | **GET** /file_response | Get a file
**GetStructuredYaml**](default_api.md#GetStructuredYaml) | **GET** /get-structured-yaml | 
**HtmlPost**](default_api.md#HtmlPost) | **POST** /html | Test HTML handling
**PostYaml**](default_api.md#PostYaml) | **POST** /post-yaml | 
**RawJsonGet**](default_api.md#RawJsonGet) | **GET** /raw_json | Get an arbitrary JSON blob.
**SoloObjectPost**](default_api.md#SoloObjectPost) | **POST** /solo-object | Send an arbitrary JSON blob


# **AllOfGet**
> models::AllOfObject AllOfGet()


Test getting an object which uses allOf

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**models::AllOfObject**](allOfObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

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

# **GetStructuredYaml**
> models::GetYamlResponse GetStructuredYaml()


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

# **PostYaml**
> PostYaml(value)


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

# **RawJsonGet**
> serde_json::Value RawJsonGet()
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

# **SoloObjectPost**
> SoloObjectPost(value)
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

