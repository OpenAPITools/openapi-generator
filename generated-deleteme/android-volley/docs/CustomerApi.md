# CustomerApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getV1CustomerOccupationCodes**](CustomerApi.md#getV1CustomerOccupationCodes) | **GET** /v1/customer/occupation-codes | Get occupation code options
[**getV1CustomerSourcesOfWealth**](CustomerApi.md#getV1CustomerSourcesOfWealth) | **GET** /v1/customer/sources-of-wealth | List of sources of wealth options



## getV1CustomerOccupationCodes

> OccupationCodeListResponse getV1CustomerOccupationCodes()

Get occupation code options

Get the occupation code options available

### Example

```java
// Import classes:
//import org.openapitools.client.api.CustomerApi;

CustomerApi apiInstance = new CustomerApi();
try {
    OccupationCodeListResponse result = apiInstance.getV1CustomerOccupationCodes();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling CustomerApi#getV1CustomerOccupationCodes");
    e.printStackTrace();
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**OccupationCodeListResponse**](OccupationCodeListResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV1CustomerSourcesOfWealth

> List&lt;String&gt; getV1CustomerSourcesOfWealth()

List of sources of wealth options

Get the list of options for a customers sources of wealth

### Example

```java
// Import classes:
//import org.openapitools.client.api.CustomerApi;

CustomerApi apiInstance = new CustomerApi();
try {
    List<String> result = apiInstance.getV1CustomerSourcesOfWealth();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling CustomerApi#getV1CustomerSourcesOfWealth");
    e.printStackTrace();
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

**List&lt;String&gt;**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

