# IbLendingApplicationApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getV1ListApplications**](IbLendingApplicationApi.md#getV1ListApplications) | **GET** /v1/lending-applications | View overview of users applications
[**getV1ReadApplication**](IbLendingApplicationApi.md#getV1ReadApplication) | **GET** /v1/lending-applications/{applicationId} | View lending application detail
[**patchV1UpdateApplication**](IbLendingApplicationApi.md#patchV1UpdateApplication) | **PATCH** /v1/lending-applications/{applicationId} | Update lending application details
[**postV1CreateApplication**](IbLendingApplicationApi.md#postV1CreateApplication) | **POST** /v1/lending-applications | Open new application
[**postV1SubmitApplication**](IbLendingApplicationApi.md#postV1SubmitApplication) | **POST** /v1/lending-applications/{applicationId}/submit | Submit application to staff



## getV1ListApplications

> LendingViewApplicationsResponse getV1ListApplications()

View overview of users applications

List of an overview of each lending applications for the user.

### Example

```java
// Import classes:
//import org.openapitools.client.api.IbLendingApplicationApi;

IbLendingApplicationApi apiInstance = new IbLendingApplicationApi();
try {
    LendingViewApplicationsResponse result = apiInstance.getV1ListApplications();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling IbLendingApplicationApi#getV1ListApplications");
    e.printStackTrace();
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**LendingViewApplicationsResponse**](LendingViewApplicationsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV1ReadApplication

> LendingViewApplicationDetailsResponse getV1ReadApplication(applicationId)

View lending application detail

Read an exisiting lending application by applicationId

### Example

```java
// Import classes:
//import org.openapitools.client.api.IbLendingApplicationApi;

IbLendingApplicationApi apiInstance = new IbLendingApplicationApi();
UUID applicationId = null; // UUID | The applicationId associated with the lending application
try {
    LendingViewApplicationDetailsResponse result = apiInstance.getV1ReadApplication(applicationId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling IbLendingApplicationApi#getV1ReadApplication");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **applicationId** | [**UUID**](.md)| The applicationId associated with the lending application | [default to null]

### Return type

[**LendingViewApplicationDetailsResponse**](LendingViewApplicationDetailsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## patchV1UpdateApplication

> LendingApplicationDetails patchV1UpdateApplication(applicationId, lendingUpdateApplicationDetailsCommand)

Update lending application details

Update part of a lending application for the fields provided in the body.

### Example

```java
// Import classes:
//import org.openapitools.client.api.IbLendingApplicationApi;

IbLendingApplicationApi apiInstance = new IbLendingApplicationApi();
UUID applicationId = null; // UUID | The applicationId associated with the lending application
LendingUpdateApplicationDetailsCommand lendingUpdateApplicationDetailsCommand = {"details":{"applicant":{"firstName":"Senor","middleName":"","lastName":"Pineapple","dateOfBirth":"2019-08-24","residentialAddress":{"addressLine1":"15 Underthesea Street","addressLine2":"","addressType":"RESIDENTIAL","postCode":"2298","state":"NSW","suburb":"Hamilton"},"postalAddress":{"addressLine1":"15 Underthesea Street","addressLine2":"","addressType":"RESIDENTIAL","postCode":"2298","state":"NSW","suburb":"Hamilton"},"mobileNumber":"0123456789","emailAddress":"user@example.com","customerId":"string","termsAndConditionsAccepted":true},"product":{"productType":"PERSONAL_LOAN","productDetails":{"loanPurpose":"CAR","loanAmount":1,"secured":true}},"state":{"status":"NEW","statusLastChanged":"2019-08-24T14:15:22Z"}}}; // LendingUpdateApplicationDetailsCommand | New values for the details
try {
    LendingApplicationDetails result = apiInstance.patchV1UpdateApplication(applicationId, lendingUpdateApplicationDetailsCommand);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling IbLendingApplicationApi#patchV1UpdateApplication");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **applicationId** | [**UUID**](.md)| The applicationId associated with the lending application | [default to null]
 **lendingUpdateApplicationDetailsCommand** | [**LendingUpdateApplicationDetailsCommand**](LendingUpdateApplicationDetailsCommand.md)| New values for the details | [optional]

### Return type

[**LendingApplicationDetails**](LendingApplicationDetails.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1CreateApplication

> LendingApplicationOverview postV1CreateApplication(lendingNewApplicationCommand)

Open new application

Create new lending application record linked to a user.

### Example

```java
// Import classes:
//import org.openapitools.client.api.IbLendingApplicationApi;

IbLendingApplicationApi apiInstance = new IbLendingApplicationApi();
LendingNewApplicationCommand lendingNewApplicationCommand = {"termsAndConditionsAccepted":true}; // LendingNewApplicationCommand | 
try {
    LendingApplicationOverview result = apiInstance.postV1CreateApplication(lendingNewApplicationCommand);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling IbLendingApplicationApi#postV1CreateApplication");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **lendingNewApplicationCommand** | [**LendingNewApplicationCommand**](LendingNewApplicationCommand.md)|  | [optional]

### Return type

[**LendingApplicationOverview**](LendingApplicationOverview.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV1SubmitApplication

> LendingSubmitApplicationResponse postV1SubmitApplication(applicationId)

Submit application to staff

Submit lending application to staff member.

### Example

```java
// Import classes:
//import org.openapitools.client.api.IbLendingApplicationApi;

IbLendingApplicationApi apiInstance = new IbLendingApplicationApi();
String applicationId = null; // String | 
try {
    LendingSubmitApplicationResponse result = apiInstance.postV1SubmitApplication(applicationId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling IbLendingApplicationApi#postV1SubmitApplication");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **applicationId** | **String**|  | [default to null]

### Return type

[**LendingSubmitApplicationResponse**](LendingSubmitApplicationResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

