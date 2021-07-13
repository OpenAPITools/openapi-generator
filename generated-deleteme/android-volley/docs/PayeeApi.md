# PayeeApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getV1Payees**](PayeeApi.md#getV1Payees) | **GET** /v1/payee | Your GET endpoint



## getV1Payees

> GetCustomerPayeesResponse getV1Payees()

Your GET endpoint

Returns a list of payees the customer has saved

### Example

```java
// Import classes:
//import org.openapitools.client.api.PayeeApi;

PayeeApi apiInstance = new PayeeApi();
try {
    GetCustomerPayeesResponse result = apiInstance.getV1Payees();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling PayeeApi#getV1Payees");
    e.printStackTrace();
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**GetCustomerPayeesResponse**](GetCustomerPayeesResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

