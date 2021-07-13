# UserAnonymousApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getCustomer**](UserAnonymousApi.md#getCustomer) | **GET** /v1/users | Look up if an existing customer with this username exists.
[**getEnvironmentEnabledFeatures**](UserAnonymousApi.md#getEnvironmentEnabledFeatures) | **GET** /v1/features | Get environment features.
[**postV1GenerateOtpForForgottenAccessCode**](UserAnonymousApi.md#postV1GenerateOtpForForgottenAccessCode) | **POST** /v1/user/forgotten-access-code/generate-otp | Send request to generate OTP that can be used to reset forgotten access code
[**postV1GenerateOtpForForgottenPassword**](UserAnonymousApi.md#postV1GenerateOtpForForgottenPassword) | **POST** /v1/user/forgotten-password/generate-otp | Send request to generate OTP that can be used to reset forgotten password
[**postV1ResetForgottenAccessCode**](UserAnonymousApi.md#postV1ResetForgottenAccessCode) | **POST** /v1/user/forgotten-access-code/reset | Send request to validate OTP and reset access code
[**postV1ResetForgottenPassword**](UserAnonymousApi.md#postV1ResetForgottenPassword) | **POST** /v1/user/forgotten-password/reset | Send request to validate OTP and reset password



## getCustomer

> CustomerGetResponseBody getCustomer(username)

Look up if an existing customer with this username exists.

Look up if an existing customer with this username exists

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserAnonymousApi;

UserAnonymousApi apiInstance = new UserAnonymousApi();
String username = 0467583902; // String | Username
try {
    CustomerGetResponseBody result = apiInstance.getCustomer(username);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling UserAnonymousApi#getCustomer");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| Username | [default to null]

### Return type

[**CustomerGetResponseBody**](CustomerGetResponseBody.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getEnvironmentEnabledFeatures

> GetFeatures getEnvironmentEnabledFeatures()

Get environment features.

# Get environment features    Gets a list of application features all users on the environment are allowed to access  

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserAnonymousApi;

UserAnonymousApi apiInstance = new UserAnonymousApi();
try {
    GetFeatures result = apiInstance.getEnvironmentEnabledFeatures();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling UserAnonymousApi#getEnvironmentEnabledFeatures");
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


## postV1GenerateOtpForForgottenAccessCode

> GenerateOtpForForgottenAccessCodeResponse postV1GenerateOtpForForgottenAccessCode(generateOtpForForgottenAccessCodeCommand)

Send request to generate OTP that can be used to reset forgotten access code

Generate OTP for forgotten access code

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserAnonymousApi;

UserAnonymousApi apiInstance = new UserAnonymousApi();
GenerateOtpForForgottenAccessCodeCommand generateOtpForForgottenAccessCodeCommand = new GenerateOtpForForgottenAccessCodeCommand(); // GenerateOtpForForgottenAccessCodeCommand | command
try {
    GenerateOtpForForgottenAccessCodeResponse result = apiInstance.postV1GenerateOtpForForgottenAccessCode(generateOtpForForgottenAccessCodeCommand);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling UserAnonymousApi#postV1GenerateOtpForForgottenAccessCode");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **generateOtpForForgottenAccessCodeCommand** | [**GenerateOtpForForgottenAccessCodeCommand**](GenerateOtpForForgottenAccessCodeCommand.md)| command |

### Return type

[**GenerateOtpForForgottenAccessCodeResponse**](GenerateOtpForForgottenAccessCodeResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1GenerateOtpForForgottenPassword

> GenerateOtpForForgottenPasswordResponse postV1GenerateOtpForForgottenPassword(generateOtpForForgottenPasswordCommand)

Send request to generate OTP that can be used to reset forgotten password

Generate OTP for forgotten password

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserAnonymousApi;

UserAnonymousApi apiInstance = new UserAnonymousApi();
GenerateOtpForForgottenPasswordCommand generateOtpForForgottenPasswordCommand = new GenerateOtpForForgottenPasswordCommand(); // GenerateOtpForForgottenPasswordCommand | command
try {
    GenerateOtpForForgottenPasswordResponse result = apiInstance.postV1GenerateOtpForForgottenPassword(generateOtpForForgottenPasswordCommand);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling UserAnonymousApi#postV1GenerateOtpForForgottenPassword");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **generateOtpForForgottenPasswordCommand** | [**GenerateOtpForForgottenPasswordCommand**](GenerateOtpForForgottenPasswordCommand.md)| command |

### Return type

[**GenerateOtpForForgottenPasswordResponse**](GenerateOtpForForgottenPasswordResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1ResetForgottenAccessCode

> ResetForgottenAccessCodeResponse postV1ResetForgottenAccessCode(resetForgottenAccessCodeCommand)

Send request to validate OTP and reset access code

Reset forgotten access code

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserAnonymousApi;

UserAnonymousApi apiInstance = new UserAnonymousApi();
ResetForgottenAccessCodeCommand resetForgottenAccessCodeCommand = new ResetForgottenAccessCodeCommand(); // ResetForgottenAccessCodeCommand | command
try {
    ResetForgottenAccessCodeResponse result = apiInstance.postV1ResetForgottenAccessCode(resetForgottenAccessCodeCommand);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling UserAnonymousApi#postV1ResetForgottenAccessCode");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **resetForgottenAccessCodeCommand** | [**ResetForgottenAccessCodeCommand**](ResetForgottenAccessCodeCommand.md)| command |

### Return type

[**ResetForgottenAccessCodeResponse**](ResetForgottenAccessCodeResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1ResetForgottenPassword

> postV1ResetForgottenPassword(resetForgottenPasswordCommand)

Send request to validate OTP and reset password

Reset forgotten password

### Example

```java
// Import classes:
//import org.openapitools.client.api.UserAnonymousApi;

UserAnonymousApi apiInstance = new UserAnonymousApi();
ResetForgottenPasswordCommand resetForgottenPasswordCommand = new ResetForgottenPasswordCommand(); // ResetForgottenPasswordCommand | command
try {
    apiInstance.postV1ResetForgottenPassword(resetForgottenPasswordCommand);
} catch (ApiException e) {
    System.err.println("Exception when calling UserAnonymousApi#postV1ResetForgottenPassword");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **resetForgottenPasswordCommand** | [**ResetForgottenPasswordCommand**](ResetForgottenPasswordCommand.md)| command |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

