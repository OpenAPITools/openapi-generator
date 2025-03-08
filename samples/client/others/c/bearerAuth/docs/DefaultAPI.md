# DefaultAPI

All URIs are relative to *http://api.example.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**DefaultAPI_privateGet**](DefaultAPI.md#DefaultAPI_privateGet) | **GET** /private | Returns private information.
[**DefaultAPI_publicGet**](DefaultAPI.md#DefaultAPI_publicGet) | **GET** /public | Returns public information.
[**DefaultAPI_usersGet**](DefaultAPI.md#DefaultAPI_usersGet) | **GET** /users | Returns a list of users.


# **DefaultAPI_privateGet**
```c
// Returns private information.
//
// This endpoint requires global security settings.
//
object_t* DefaultAPI_privateGet(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type

[object_t](object.md) *


### Authorization

[bearerAuth](../README.md#bearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **DefaultAPI_publicGet**
```c
// Returns public information.
//
// This endpoint does not require authentication.
//
object_t* DefaultAPI_publicGet(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type

[object_t](object.md) *


### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **DefaultAPI_usersGet**
```c
// Returns a list of users.
//
// Optional extended description in CommonMark or HTML.
//
list_t* DefaultAPI_usersGet(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type


[list_t](char.md) *




### Authorization

[bearerAuth](../README.md#bearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

