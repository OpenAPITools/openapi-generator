# \FooApi

All URIs are relative to *http://localhost:8080*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_foo**](FooApi.md#create_foo) | **Post** /foo | Create a Foo
[**get_all_foos**](FooApi.md#get_all_foos) | **Get** /foo | GET all Foos



## create_foo

> models::FooRefOrValue create_foo(foo)
Create a Foo

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**foo** | Option<[**Foo**](Foo.md)> | The Foo to be created |  |

### Return type

[**models::FooRefOrValue**](FooRefOrValue.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_all_foos

> Vec<models::FooRefOrValue> get_all_foos()
GET all Foos

### Parameters

This endpoint does not need any parameter.

### Return type

[**Vec<models::FooRefOrValue>**](FooRefOrValue.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

