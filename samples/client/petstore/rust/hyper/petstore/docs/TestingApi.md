# \TestingApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**tests_file_response_get**](TestingApi.md#tests_file_response_get) | **Get** /tests/fileResponse | Returns an image file
[**tests_type_testing_get**](TestingApi.md#tests_type_testing_get) | **Get** /tests/typeTesting | Route to test the TypeTesting schema



## tests_file_response_get

> std::path::PathBuf tests_file_response_get()
Returns an image file

### Parameters

This endpoint does not need any parameter.

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: image/jpeg

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## tests_type_testing_get

> crate::models::TypeTesting tests_type_testing_get()
Route to test the TypeTesting schema

### Parameters

This endpoint does not need any parameter.

### Return type

[**crate::models::TypeTesting**](TypeTesting.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

