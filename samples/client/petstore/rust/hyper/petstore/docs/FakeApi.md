# \FakeApi

All URIs are relative to *http://localhost/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_nullable_required_param**](FakeApi.md#test_nullable_required_param) | **Get** /fake/user/{user_name} | To test nullable required parameters



## test_nullable_required_param

> test_nullable_required_param(user_name, dummy_required_nullable_param, any_type, uppercase, content)
To test nullable required parameters



### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**user_name** | **String** | The name that needs to be fetched. Use user1 for testing. | [required] |
**dummy_required_nullable_param** | Option<**String**> | To test nullable required parameters | [required] |
**any_type** | **String** |  | [required] |
**uppercase** | Option<**String**> | To test parameter names in upper case |  |
**content** | Option<**String**> | To test escaping of parameters in rust code works |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

