# \DefaultApi

All URIs are relative to *http://localhost:8080*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_shape**](DefaultApi.md#create_shape) | **Post** /shape | Create a shape
[**create_vehicle**](DefaultApi.md#create_vehicle) | **Post** /vehicle | Create a vehicle



## create_shape

> models::Shape create_shape(shape)
Create a shape

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**shape** | [**Shape**](Shape.md) |  | [required] |

### Return type

[**models::Shape**](Shape.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_vehicle

> models::Vehicle create_vehicle(vehicle)
Create a vehicle

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**vehicle** | [**Vehicle**](Vehicle.md) |  | [required] |

### Return type

[**models::Vehicle**](Vehicle.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

