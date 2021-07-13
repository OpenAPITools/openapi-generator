# OnboardingApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**postApplication**](OnboardingApi.md#postApplication) | **POST** /v1/sign-up/applications | Creates and submits an application for new customer(s) and an initial product.
[**postCustomerVerification**](OnboardingApi.md#postCustomerVerification) | **POST** /v1/sign-up/verify-customer | Submits an verification objects.
[**postPendingMobileNumber**](OnboardingApi.md#postPendingMobileNumber) | **POST** /v1/sign-up/pending-mobile | Create mobile number, pending verification.
[**postV1SignUpCustomerOccupation**](OnboardingApi.md#postV1SignUpCustomerOccupation) | **POST** /v1/sign-up/customer-occupation | Onboarding Occupation
[**postV1SignUpCustomerSourceOfWealth**](OnboardingApi.md#postV1SignUpCustomerSourceOfWealth) | **POST** /v1/sign-up/customer-source-of-wealth | Onboarding Source of Wealth
[**postVerifyMobileNumber**](OnboardingApi.md#postVerifyMobileNumber) | **POST** /v1/sign-up/verify-mobile | Verify mobile number.



## postApplication

> ApplicationPostResponseBody postApplication(applicationRequest)

Creates and submits an application for new customer(s) and an initial product.

Creates and submits an application for new customer(s) and an initial product

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingApi;

OnboardingApi apiInstance = new OnboardingApi();
ApplicationRequest applicationRequest = {"onboardingId":"8263ab26-b240-44f8-80f4-e6b9a7f5a262","applicants":[{"firstName":"Nathan","middleName":"Q","lastName":"Citizen","preferredName":"Nath","dateOfBirth":"2001-06-13","contactDetails":{"residentialAddress":{"addressLine1":"103 Tudor Street","addressLine2":"","townName":"Hamilton","countrySubDivision":"NSW","postalCode":"2303","type":"Residential address"}}}]}; // ApplicationRequest | Creates and submits an application for new customer(s) and an initial product
try {
    ApplicationPostResponseBody result = apiInstance.postApplication(applicationRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingApi#postApplication");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **applicationRequest** | [**ApplicationRequest**](ApplicationRequest.md)| Creates and submits an application for new customer(s) and an initial product | [optional]

### Return type

[**ApplicationPostResponseBody**](ApplicationPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postCustomerVerification

> CustomerVerificationPostResponseBody postCustomerVerification(customerVerificationRequest)

Submits an verification objects.

Submits an verification objects

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingApi;

OnboardingApi apiInstance = new OnboardingApi();
CustomerVerificationRequest customerVerificationRequest = {"driverLicense":[{"licenseNumber":"12345678","countrySubDivision":"NSW"}],"applicants":[{"firstName":"Nathan","middleName":"Q","lastName":"Citizen","preferredName":"Nath","dateOfBirth":"2001-06-13","contactDetails":{"residentialAddress":{"addressLine1":"103 Tudor Street","addressLine2":"","townName":"Hamilton","countrySubDivision":"NSW","postalCode":"2303","type":"Residential address"},"postalAddress":{"addressLine1":"148B Beaumont Street","addressLine2":"","townName":"Hamilton","countrySubDivision":"NSW","postalCode":"2303","type":"Postal address"}}}],"onboardingId":"a5b0fe7d-c4dd-40a7-bd80-dfc7869327e1"}; // CustomerVerificationRequest | Submits an verification objects
try {
    CustomerVerificationPostResponseBody result = apiInstance.postCustomerVerification(customerVerificationRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingApi#postCustomerVerification");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **customerVerificationRequest** | [**CustomerVerificationRequest**](CustomerVerificationRequest.md)| Submits an verification objects | [optional]

### Return type

[**CustomerVerificationPostResponseBody**](CustomerVerificationPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postPendingMobileNumber

> PendingMobileNumberPostResponseBody postPendingMobileNumber(pendingMobileNumberRequest)

Create mobile number, pending verification.

Create mobile number, pending verification

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingApi;

OnboardingApi apiInstance = new OnboardingApi();
PendingMobileNumberRequest pendingMobileNumberRequest = {"mobilePhoneNumber":"0400800800","displayName":"Jane","onboardingId":"a5b0fe7d-c4dd-40a7-bd80-dfc7869327e1"}; // PendingMobileNumberRequest | Create mobile number, pending verification
try {
    PendingMobileNumberPostResponseBody result = apiInstance.postPendingMobileNumber(pendingMobileNumberRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingApi#postPendingMobileNumber");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pendingMobileNumberRequest** | [**PendingMobileNumberRequest**](PendingMobileNumberRequest.md)| Create mobile number, pending verification | [optional]

### Return type

[**PendingMobileNumberPostResponseBody**](PendingMobileNumberPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1SignUpCustomerOccupation

> CustomerOccupationResponse postV1SignUpCustomerOccupation(customerOccupationRequest)

Onboarding Occupation

Save the onboarding users occupation

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingApi;

OnboardingApi apiInstance = new OnboardingApi();
CustomerOccupationRequest customerOccupationRequest = new CustomerOccupationRequest(); // CustomerOccupationRequest | Request to store a customers occupation during onboarding
try {
    CustomerOccupationResponse result = apiInstance.postV1SignUpCustomerOccupation(customerOccupationRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingApi#postV1SignUpCustomerOccupation");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **customerOccupationRequest** | [**CustomerOccupationRequest**](CustomerOccupationRequest.md)| Request to store a customers occupation during onboarding | [optional]

### Return type

[**CustomerOccupationResponse**](CustomerOccupationResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1SignUpCustomerSourceOfWealth

> CustomerSourceOfWealthResponse postV1SignUpCustomerSourceOfWealth(customerSourceOfWealthRequest)

Onboarding Source of Wealth

Save the onboarding users source of wealth

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingApi;

OnboardingApi apiInstance = new OnboardingApi();
CustomerSourceOfWealthRequest customerSourceOfWealthRequest = new CustomerSourceOfWealthRequest(); // CustomerSourceOfWealthRequest | Request to store a customers source of wealth during onboarding
try {
    CustomerSourceOfWealthResponse result = apiInstance.postV1SignUpCustomerSourceOfWealth(customerSourceOfWealthRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingApi#postV1SignUpCustomerSourceOfWealth");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **customerSourceOfWealthRequest** | [**CustomerSourceOfWealthRequest**](CustomerSourceOfWealthRequest.md)| Request to store a customers source of wealth during onboarding | [optional]

### Return type

[**CustomerSourceOfWealthResponse**](CustomerSourceOfWealthResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postVerifyMobileNumber

> VerifyMobileNumberPostResponseBody postVerifyMobileNumber(verifyMobileNumberRequest)

Verify mobile number.

Verify mobile number

### Example

```java
// Import classes:
//import org.openapitools.client.api.OnboardingApi;

OnboardingApi apiInstance = new OnboardingApi();
VerifyMobileNumberRequest verifyMobileNumberRequest = {"onboardingId":"a5b0fe7d-c4dd-40a7-bd80-dfc7869327e1","otpUuid":"909ab4d6-0e36-4e76-b218-4af1bf00e53f","otp":"123456"}; // VerifyMobileNumberRequest | Verify mobile number
try {
    VerifyMobileNumberPostResponseBody result = apiInstance.postVerifyMobileNumber(verifyMobileNumberRequest);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling OnboardingApi#postVerifyMobileNumber");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **verifyMobileNumberRequest** | [**VerifyMobileNumberRequest**](VerifyMobileNumberRequest.md)| Verify mobile number | [optional]

### Return type

[**VerifyMobileNumberPostResponseBody**](VerifyMobileNumberPostResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

