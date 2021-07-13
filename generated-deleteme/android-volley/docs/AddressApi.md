# AddressApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getLookupAddress**](AddressApi.md#getLookupAddress) | **GET** /v1/address/lookup | Get a list of addresses matching provided query.
[**getValidateAddress**](AddressApi.md#getValidateAddress) | **GET** /v1/address/validate | Get a validated address.



## getLookupAddress

> AddressList getLookupAddress(address)

Get a list of addresses matching provided query.

Get a list of addresses matching provided query

### Example

```java
// Import classes:
//import org.openapitools.client.api.AddressApi;

AddressApi apiInstance = new AddressApi();
String address = 4; // String | Single line address filter
try {
    AddressList result = apiInstance.getLookupAddress(address);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AddressApi#getLookupAddress");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **address** | **String**| Single line address filter | [default to null]

### Return type

[**AddressList**](AddressList.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getValidateAddress

> ValidatedAddress getValidateAddress(address)

Get a validated address.

Get a validated address

### Example

```java
// Import classes:
//import org.openapitools.client.api.AddressApi;

AddressApi apiInstance = new AddressApi();
String address = 4; // String | No description available
try {
    ValidatedAddress result = apiInstance.getValidateAddress(address);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AddressApi#getValidateAddress");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **address** | **String**| No description available | [default to null]

### Return type

[**ValidatedAddress**](ValidatedAddress.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

