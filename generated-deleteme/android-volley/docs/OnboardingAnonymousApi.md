# OnboardingAnonymousApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**postCustomer**](OnboardingAnonymousApi.md#postCustomer) | **POST** /v1/sign-up | Create initial customer record.
[**postPendingUser**](OnboardingAnonymousApi.md#postPendingUser) | **POST** /v1/sign-up/pending-user | Create pending user.
[**postTermsAndConditions**](OnboardingAnonymousApi.md#postTermsAndConditions) | **POST** /v1/sign-up/terms-and-conditions | Customer accepting terms and conditions.
[**postVerifyUser**](OnboardingAnonymousApi.md#postVerifyUser) | **POST** /v1/sign-up/verify-user | Verify user.



## postCustomer

> CustomerPostResponseBody postCustomer(customerRequest)

Create initial customer record.

Create initial customer record

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingAnonymousApi;

OnboardingAnonymousApi apiInstance = new OnboardingAnonymousApi();
CustomerRequest customerRequest = {"totp":"123456"}; // CustomerRequest | Create initial customer record
try {
    CustomerPostResponseBody result = apiInstance.postCustomer(customerRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingAnonymousApi#postCustomer");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **customerRequest** | [**CustomerRequest**](CustomerRequest.md)| Create initial customer record | [optional]

### Return type

[**CustomerPostResponseBody**](CustomerPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postPendingUser

> postPendingUser(pendingUserRequest)

Create pending user.

Create pending user

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingAnonymousApi;

OnboardingAnonymousApi apiInstance = new OnboardingAnonymousApi();
PendingUserRequest pendingUserRequest = {"userName":"0400800800","displayName":"Jane","onboardingId":"a5b0fe7d-c4dd-40a7-bd80-dfc7869327e1"}; // PendingUserRequest | Create pending user
try {
    apiInstance.postPendingUser(pendingUserRequest);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingAnonymousApi#postPendingUser");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pendingUserRequest** | [**PendingUserRequest**](PendingUserRequest.md)| Create pending user | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postTermsAndConditions

> TermsAndConditionsPostResponseBody postTermsAndConditions(termsAndConditionsRequest)

Customer accepting terms and conditions.

Customer accepting terms and conditions

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingAnonymousApi;

OnboardingAnonymousApi apiInstance = new OnboardingAnonymousApi();
TermsAndConditionsRequest termsAndConditionsRequest = {"onboardingId":"a5b0fe7d-c4dd-40a7-bd80-dfc7869327e1"}; // TermsAndConditionsRequest | Customer accepting terms and conditions
try {
    TermsAndConditionsPostResponseBody result = apiInstance.postTermsAndConditions(termsAndConditionsRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingAnonymousApi#postTermsAndConditions");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **termsAndConditionsRequest** | [**TermsAndConditionsRequest**](TermsAndConditionsRequest.md)| Customer accepting terms and conditions | [optional]

### Return type

[**TermsAndConditionsPostResponseBody**](TermsAndConditionsPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postVerifyUser

> VerifyUserPostResponseBody postVerifyUser(verifyUserRequest)

Verify user.

Verify user

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingAnonymousApi;

OnboardingAnonymousApi apiInstance = new OnboardingAnonymousApi();
VerifyUserRequest verifyUserRequest = {"userName":"0400800800","password":"blah","onboardingId":"a5b0fe7d-c4dd-40a7-bd80-dfc7869327e1","otp":"123456"}; // VerifyUserRequest | Verify user
try {
    VerifyUserPostResponseBody result = apiInstance.postVerifyUser(verifyUserRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingAnonymousApi#postVerifyUser");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **verifyUserRequest** | [**VerifyUserRequest**](VerifyUserRequest.md)| Verify user | [optional]

### Return type

[**VerifyUserPostResponseBody**](VerifyUserPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

