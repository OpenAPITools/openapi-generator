# repo_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
**CreateRepo**](repo_api.md#CreateRepo) | **POST** /repos | 
**GetRepoInfo**](repo_api.md#GetRepoInfo) | **GET** /repos/{repoId} | 


# **CreateRepo**
> CreateRepo(object_param)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **object_param** | [**ObjectParam**](ObjectParam.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetRepoInfo**
> String GetRepoInfo(repo_id)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **repo_id** | **String**|  | 

### Return type

[**String**](string.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

