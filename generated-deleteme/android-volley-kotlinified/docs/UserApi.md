# UserApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getCurrentUserEnabledFeatures**](UserApi.md#getCurrentUserEnabledFeatures) | **GET** /v1/users/me/features | Get user features.



## getCurrentUserEnabledFeatures

> GetFeatures getCurrentUserEnabledFeatures()

Get user features.

# Get user features    Gets a list of application features the user is allowed to access  

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserApi;

UserApi apiInstance = new UserApi();
try {
    GetFeatures result = apiInstance.getCurrentUserEnabledFeatures();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling UserApi#getCurrentUserEnabledFeatures");
    e.printStackTrace();
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**GetFeatures**](GetFeatures.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

