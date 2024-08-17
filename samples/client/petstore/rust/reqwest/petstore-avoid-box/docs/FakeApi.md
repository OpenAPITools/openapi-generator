# \FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_nullable_required_param**](FakeApi.md#test_nullable_required_param) | **GET** /fake/user/{username} | To test nullable required parameters



## test_nullable_required_param

> test_nullable_required_param(username, dummy_required_nullable_param, uppercase)
To test nullable required parameters



### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**username** | **String** | The name that needs to be fetched. Use user1 for testing. | [required] |
**dummy_required_nullable_param** | Option<**String**> | To test nullable required parameters | [required] |
**uppercase** | Option<**String**> | To test parameter names in upper case |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

