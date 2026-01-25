# \TestingApi

All URIs are relative to *http://localhost/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**tests_all_of_with_one_model_get**](TestingApi.md#tests_all_of_with_one_model_get) | **GET** /tests/allOfWithOneModel | Test for allOf with a single option. (One of the issues in #20500)
[**tests_file_response_get**](TestingApi.md#tests_file_response_get) | **GET** /tests/fileResponse | Returns an image file
[**tests_inline_enum_boxing_get**](TestingApi.md#tests_inline_enum_boxing_get) | **GET** /tests/inlineEnumBoxing | Get model with inline enums
[**tests_inline_enum_boxing_post**](TestingApi.md#tests_inline_enum_boxing_post) | **POST** /tests/inlineEnumBoxing | Test for inline enum fields not being boxed in model constructors
[**tests_type_testing_get**](TestingApi.md#tests_type_testing_get) | **GET** /tests/typeTesting | Route to test the TypeTesting schema



## tests_all_of_with_one_model_get

> String tests_all_of_with_one_model_get(foo_person)
Test for allOf with a single option. (One of the issues in #20500)

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**foo_person** | [**FooPerson**](FooPerson.md) |  | [required] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


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


## tests_inline_enum_boxing_get

> Vec<models::FooModelWithInlineEnum> tests_inline_enum_boxing_get(status)
Get model with inline enums

Tests inline enum query parameters

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**status** | Option<**String**> | Filter by status (inline enum) |  |

### Return type

[**Vec<models::FooModelWithInlineEnum>**](ModelWithInlineEnum.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## tests_inline_enum_boxing_post

> models::FooModelWithInlineEnum tests_inline_enum_boxing_post(foo_model_with_inline_enum)
Test for inline enum fields not being boxed in model constructors

Regression test to ensure inline enum fields are not wrapped in Box::new() in model constructors

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**foo_model_with_inline_enum** | [**FooModelWithInlineEnum**](FooModelWithInlineEnum.md) |  | [required] |

### Return type

[**models::FooModelWithInlineEnum**](ModelWithInlineEnum.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## tests_type_testing_get

> models::FooTypeTesting tests_type_testing_get()
Route to test the TypeTesting schema

### Parameters

This endpoint does not need any parameter.

### Return type

[**models::FooTypeTesting**](TypeTesting.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

