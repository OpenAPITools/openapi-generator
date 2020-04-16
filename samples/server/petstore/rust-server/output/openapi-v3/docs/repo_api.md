# repo_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
**CreateRepo**](repo_api.md#CreateRepo) | **POST** /repos | 
**GetRepoInfo**](repo_api.md#GetRepoInfo) | **GET** /repos/{repoId} | 


# **CreateRepo**
> models::Error CreateRepo(object_param)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **object_param** | [**ObjectParam**](ObjectParam.md)|  | 

### Return type

[**models::Error**](Error.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetRepoInfo**
> serde_json::Value GetRepoInfo(repo_id)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **repo_id** | **String**|  | 

### Return type

[**serde_json::Value**](object.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

