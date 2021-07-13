# NppApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getPayment**](NppApi.md#getPayment) | **GET** /v1/npp/payment | Get a NPP payment for a given paymentId.
[**getRejectedPayments**](NppApi.md#getRejectedPayments) | **GET** /v1/npp/payments/{accountId}/rejected | Gets any rejected NPP payments from the past 30 days for a specific account.
[**getResolveAccount**](NppApi.md#getResolveAccount) | **GET** /v1/npp/resolve/account | NPP Resolve Account.
[**getResolveAlias**](NppApi.md#getResolveAlias) | **GET** /v1/npp/resolve/alias | NPP Resolve Alias.
[**postPaymentAccount**](NppApi.md#postPaymentAccount) | **POST** /v1/npp/payment/account | 
[**postPaymentAlias**](NppApi.md#postPaymentAlias) | **POST** /v1/npp/payment/alias | 



## getPayment

> NppPaymentGetResponse getPayment(paymentId, accountId)

Get a NPP payment for a given paymentId.

Get a NPP payment for a given paymentId

### Example

```java
// Import classes:
//import org.openapitools.client.api.NppApi;

NppApi apiInstance = new NppApi();
String paymentId = c0ff9c15-d555-467d-9955-703f91ea834b; // String | The unique payment identifier
String accountId = SAV-100003198; // String | The associated account id which the payment is about
try {
    NppPaymentGetResponse result = apiInstance.getPayment(paymentId, accountId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling NppApi#getPayment");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **paymentId** | **String**| The unique payment identifier | [default to null]
 **accountId** | **String**| The associated account id which the payment is about | [default to null]

### Return type

[**NppPaymentGetResponse**](NppPaymentGetResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getRejectedPayments

> NppRejectedPaymentsGetResponse getRejectedPayments(accountId)

Gets any rejected NPP payments from the past 30 days for a specific account.

Gets any rejected NPP payments from the past 30 days for a specific account.

### Example

```java
// Import classes:
//import org.openapitools.client.api.NppApi;

NppApi apiInstance = new NppApi();
String accountId = SAV-100002612; // String | The ID of the account for which rejected payments will be retrieved
try {
    NppRejectedPaymentsGetResponse result = apiInstance.getRejectedPayments(accountId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling NppApi#getRejectedPayments");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountId** | **String**| The ID of the account for which rejected payments will be retrieved | [default to null]

### Return type

[**NppRejectedPaymentsGetResponse**](NppRejectedPaymentsGetResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getResolveAccount

> NppResolveAccountGetResponse getResolveAccount(accountBsb, accountNumber, overlay)

NPP Resolve Account.

Provides functionality for determining whether or not a specified account supports a specified payment overlay.

### Example

```java
// Import classes:
//import org.openapitools.client.api.NppApi;

NppApi apiInstance = new NppApi();
String accountBsb = 680000; // String | The BSB number of the account
String accountNumber = 1020304080; // String | The account number of the account
String overlay = sct; // String | This request will assess whether or not the specified account supports this overlay
try {
    NppResolveAccountGetResponse result = apiInstance.getResolveAccount(accountBsb, accountNumber, overlay);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling NppApi#getResolveAccount");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountBsb** | **String**| The BSB number of the account | [default to null]
 **accountNumber** | **String**| The account number of the account | [default to null]
 **overlay** | **String**| This request will assess whether or not the specified account supports this overlay | [default to null]

### Return type

[**NppResolveAccountGetResponse**](NppResolveAccountGetResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getResolveAlias

> NppResolveAliasGetResponse getResolveAlias(identifier, type)

NPP Resolve Alias.

Provides a lookup for PayIDAlias

### Example

```java
// Import classes:
//import org.openapitools.client.api.NppApi;

NppApi apiInstance = new NppApi();
String identifier = +61-421656716; // String | The PayID identifier to be used for the lookup. For example phone number.
String type = Phone; // String | The PayID identifier type to be used for the lookup. For example Phone.
try {
    NppResolveAliasGetResponse result = apiInstance.getResolveAlias(identifier, type);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling NppApi#getResolveAlias");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **identifier** | **String**| The PayID identifier to be used for the lookup. For example phone number. | [default to null]
 **type** | **String**| The PayID identifier type to be used for the lookup. For example Phone. | [default to Phone] [enum: Phone]

### Return type

[**NppResolveAliasGetResponse**](NppResolveAliasGetResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## postPaymentAccount

> NppPaymentsAccountPostResponse postPaymentAccount(nppPaymentsAccountPostRequest)



No description available

### Example

```java
// Import classes:
//import org.openapitools.client.api.NppApi;

NppApi apiInstance = new NppApi();
NppPaymentsAccountPostRequest nppPaymentsAccountPostRequest = new NppPaymentsAccountPostRequest(); // NppPaymentsAccountPostRequest | 
try {
    NppPaymentsAccountPostResponse result = apiInstance.postPaymentAccount(nppPaymentsAccountPostRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling NppApi#postPaymentAccount");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **nppPaymentsAccountPostRequest** | [**NppPaymentsAccountPostRequest**](NppPaymentsAccountPostRequest.md)|  | [optional]

### Return type

[**NppPaymentsAccountPostResponse**](NppPaymentsAccountPostResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postPaymentAlias

> NppPaymentsAliasPostResponse postPaymentAlias(nppPaymentsAliasPostRequest)



No description available

### Example

```java
// Import classes:
//import org.openapitools.client.api.NppApi;

NppApi apiInstance = new NppApi();
NppPaymentsAliasPostRequest nppPaymentsAliasPostRequest = new NppPaymentsAliasPostRequest(); // NppPaymentsAliasPostRequest | 
try {
    NppPaymentsAliasPostResponse result = apiInstance.postPaymentAlias(nppPaymentsAliasPostRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling NppApi#postPaymentAlias");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **nppPaymentsAliasPostRequest** | [**NppPaymentsAliasPostRequest**](NppPaymentsAliasPostRequest.md)|  | [optional]

### Return type

[**NppPaymentsAliasPostResponse**](NppPaymentsAliasPostResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

