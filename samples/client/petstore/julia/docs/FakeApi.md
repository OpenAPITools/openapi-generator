# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**uuid_default_value**](FakeApi.md#uuid_default_value) | **GET** /fake/uuid_default_value_test | test uuid default value


# **uuid_default_value**
> uuid_default_value(_api::FakeApi, uuid_parameter::String; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> uuid_default_value(_api::FakeApi, response_stream::Channel, uuid_parameter::String; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

test uuid default value

test uuid default value

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **FakeApi** | API context | 
**uuid_parameter** | **String**| test uuid default value | [default to nothing]

### Return type

Nothing

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

