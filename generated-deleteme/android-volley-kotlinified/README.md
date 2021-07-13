# au.com.greater.ibi

## Requirements

Building the API client library requires [Maven](https://maven.apache.org/) to be installed.

## Installation

To install the API client library to your local Maven repository, simply execute:

```shell
mvn install
```

To deploy it to a remote Maven repository instead, configure the settings of the repository and execute:

```shell
mvn deploy
```

Refer to the [official documentation](https://maven.apache.org/plugins/maven-deploy-plugin/usage.html) for more information.

### Maven users

Add this dependency to your project's POM:

```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>au.com.greater.ibi</artifactId>
    <version>1.0.0</version>
    <scope>compile</scope>
</dependency>
```

### Gradle users

Add this dependency to your project's build file:

```groovy
compile "org.openapitools:au.com.greater.ibi:1.0.0"
```

### Others

At first generate the JAR by executing:

    mvn package

Then manually install the following JARs:

- target/au.com.greater.ibi-1.0.0.jar
- target/lib/*.jar

## Getting Started

Please follow the [installation](#installation) instruction and execute the following Java code:

```java

import org.openapitools.client.api.AccountApi;

public class AccountApiExample {

    public static void main(String[] args) {
        AccountApi apiInstance = new AccountApi();
        String inviteCode = null; // String | 
        try {
            apiInstance.deleteV1AccountInviteInviteCode(inviteCode);
        } catch (ApiException e) {
            System.err.println("Exception when calling AccountApi#deleteV1AccountInviteInviteCode");
            e.printStackTrace();
        }
    }
}

```

## Documentation for API Endpoints

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AccountApi* | [**deleteV1AccountInviteInviteCode**](docs/AccountApi.md#deleteV1AccountInviteInviteCode) | **DELETE** /v1/account-invite/{inviteCode} | Delete an account invite
*AccountApi* | [**getV1AccountsAccountIdPendingTransactions**](docs/AccountApi.md#getV1AccountsAccountIdPendingTransactions) | **GET** /v1/accounts/{accountId}/pending-transactions | A list of pending transactions
*AccountApi* | [**getV1InviteCode**](docs/AccountApi.md#getV1InviteCode) | **GET** /v1/account-invite/{inviteCode} | GET request for joint account invitation
*AccountApi* | [**getV2PendingTransactions**](docs/AccountApi.md#getV2PendingTransactions) | **GET** /v2/accounts/{accountId}/pending-transactions | A list of pending transactions filtered by description (if provided)
*AccountApi* | [**getV3Transactions**](docs/AccountApi.md#getV3Transactions) | **GET** /v3/accounts/{accountId}/transactions | Get a list transactions
*AccountApi* | [**getV3TransactionsPaged**](docs/AccountApi.md#getV3TransactionsPaged) | **GET** /v3/accounts/{accountId}/transactions/paginated | Get a list transactions, returned as paged results.
*AccountApi* | [**postV1AccountInviteRegenerate**](docs/AccountApi.md#postV1AccountInviteRegenerate) | **POST** /v1/account-invite/{inviteId}/regenerate | Regenerate an invite code for an existing invite
*AccountApi* | [**postV1InviteCodeAccept**](docs/AccountApi.md#postV1InviteCodeAccept) | **POST** /v1/account-invite/{inviteId}/accept | 
*AccountApi* | [**postV3AccountsInvite**](docs/AccountApi.md#postV3AccountsInvite) | **POST** /v3/accounts/{accountId}/invite | Invite to a joint account
*AddressApi* | [**getLookupAddress**](docs/AddressApi.md#getLookupAddress) | **GET** /v1/address/lookup | Get a list of addresses matching provided query.
*AddressApi* | [**getValidateAddress**](docs/AddressApi.md#getValidateAddress) | **GET** /v1/address/validate | Get a validated address.
*CustomerApi* | [**getV1CustomerOccupationCodes**](docs/CustomerApi.md#getV1CustomerOccupationCodes) | **GET** /v1/customer/occupation-codes | Get occupation code options
*CustomerApi* | [**getV1CustomerSourcesOfWealth**](docs/CustomerApi.md#getV1CustomerSourcesOfWealth) | **GET** /v1/customer/sources-of-wealth | List of sources of wealth options
*IbLendingApplicationApi* | [**getV1ListApplications**](docs/IbLendingApplicationApi.md#getV1ListApplications) | **GET** /v1/lending-applications | View overview of users applications
*IbLendingApplicationApi* | [**getV1ReadApplication**](docs/IbLendingApplicationApi.md#getV1ReadApplication) | **GET** /v1/lending-applications/{applicationId} | View lending application detail
*IbLendingApplicationApi* | [**patchV1UpdateApplication**](docs/IbLendingApplicationApi.md#patchV1UpdateApplication) | **PATCH** /v1/lending-applications/{applicationId} | Update lending application details
*IbLendingApplicationApi* | [**postV1CreateApplication**](docs/IbLendingApplicationApi.md#postV1CreateApplication) | **POST** /v1/lending-applications | Open new application
*IbLendingApplicationApi* | [**postV1SubmitApplication**](docs/IbLendingApplicationApi.md#postV1SubmitApplication) | **POST** /v1/lending-applications/{applicationId}/submit | Submit application to staff
*NppApi* | [**getPayment**](docs/NppApi.md#getPayment) | **GET** /v1/npp/payment | Get a NPP payment for a given paymentId.
*NppApi* | [**getRejectedPayments**](docs/NppApi.md#getRejectedPayments) | **GET** /v1/npp/payments/{accountId}/rejected | Gets any rejected NPP payments from the past 30 days for a specific account.
*NppApi* | [**getResolveAccount**](docs/NppApi.md#getResolveAccount) | **GET** /v1/npp/resolve/account | NPP Resolve Account.
*NppApi* | [**getResolveAlias**](docs/NppApi.md#getResolveAlias) | **GET** /v1/npp/resolve/alias | NPP Resolve Alias.
*NppApi* | [**postPaymentAccount**](docs/NppApi.md#postPaymentAccount) | **POST** /v1/npp/payment/account | 
*NppApi* | [**postPaymentAlias**](docs/NppApi.md#postPaymentAlias) | **POST** /v1/npp/payment/alias | 
*OnboardingApi* | [**postApplication**](docs/OnboardingApi.md#postApplication) | **POST** /v1/sign-up/applications | Creates and submits an application for new customer(s) and an initial product.
*OnboardingApi* | [**postCustomerVerification**](docs/OnboardingApi.md#postCustomerVerification) | **POST** /v1/sign-up/verify-customer | Submits an verification objects.
*OnboardingApi* | [**postPendingMobileNumber**](docs/OnboardingApi.md#postPendingMobileNumber) | **POST** /v1/sign-up/pending-mobile | Create mobile number, pending verification.
*OnboardingApi* | [**postV1SignUpCustomerOccupation**](docs/OnboardingApi.md#postV1SignUpCustomerOccupation) | **POST** /v1/sign-up/customer-occupation | Onboarding Occupation
*OnboardingApi* | [**postV1SignUpCustomerSourceOfWealth**](docs/OnboardingApi.md#postV1SignUpCustomerSourceOfWealth) | **POST** /v1/sign-up/customer-source-of-wealth | Onboarding Source of Wealth
*OnboardingApi* | [**postVerifyMobileNumber**](docs/OnboardingApi.md#postVerifyMobileNumber) | **POST** /v1/sign-up/verify-mobile | Verify mobile number.
*OnboardingAnonymousApi* | [**postCustomer**](docs/OnboardingAnonymousApi.md#postCustomer) | **POST** /v1/sign-up | Create initial customer record.
*OnboardingAnonymousApi* | [**postPendingUser**](docs/OnboardingAnonymousApi.md#postPendingUser) | **POST** /v1/sign-up/pending-user | Create pending user.
*OnboardingAnonymousApi* | [**postTermsAndConditions**](docs/OnboardingAnonymousApi.md#postTermsAndConditions) | **POST** /v1/sign-up/terms-and-conditions | Customer accepting terms and conditions.
*OnboardingAnonymousApi* | [**postVerifyUser**](docs/OnboardingAnonymousApi.md#postVerifyUser) | **POST** /v1/sign-up/verify-user | Verify user.
*PayeeApi* | [**getV1Payees**](docs/PayeeApi.md#getV1Payees) | **GET** /v1/payee | Your GET endpoint
*UserApi* | [**getCurrentUserEnabledFeatures**](docs/UserApi.md#getCurrentUserEnabledFeatures) | **GET** /v1/users/me/features | Get user features.
*UserAnonymousApi* | [**getCustomer**](docs/UserAnonymousApi.md#getCustomer) | **GET** /v1/users | Look up if an existing customer with this username exists.
*UserAnonymousApi* | [**getEnvironmentEnabledFeatures**](docs/UserAnonymousApi.md#getEnvironmentEnabledFeatures) | **GET** /v1/features | Get environment features.
*UserAnonymousApi* | [**postV1GenerateOtpForForgottenAccessCode**](docs/UserAnonymousApi.md#postV1GenerateOtpForForgottenAccessCode) | **POST** /v1/user/forgotten-access-code/generate-otp | Send request to generate OTP that can be used to reset forgotten access code
*UserAnonymousApi* | [**postV1GenerateOtpForForgottenPassword**](docs/UserAnonymousApi.md#postV1GenerateOtpForForgottenPassword) | **POST** /v1/user/forgotten-password/generate-otp | Send request to generate OTP that can be used to reset forgotten password
*UserAnonymousApi* | [**postV1ResetForgottenAccessCode**](docs/UserAnonymousApi.md#postV1ResetForgottenAccessCode) | **POST** /v1/user/forgotten-access-code/reset | Send request to validate OTP and reset access code
*UserAnonymousApi* | [**postV1ResetForgottenPassword**](docs/UserAnonymousApi.md#postV1ResetForgottenPassword) | **POST** /v1/user/forgotten-password/reset | Send request to validate OTP and reset password


## Documentation for Models

 - [AcceptInviteRequest](docs/AcceptInviteRequest.md)
 - [Account](docs/Account.md)
 - [AccountBSBPayee](docs/AccountBSBPayee.md)
 - [AccountBSBPayeeAllOf](docs/AccountBSBPayeeAllOf.md)
 - [AccountInvite](docs/AccountInvite.md)
 - [AccountInviteResponse](docs/AccountInviteResponse.md)
 - [AddressList](docs/AddressList.md)
 - [Alias](docs/Alias.md)
 - [AliasPayee](docs/AliasPayee.md)
 - [AliasPayeeAllOf](docs/AliasPayeeAllOf.md)
 - [ApplicationPostResponseBody](docs/ApplicationPostResponseBody.md)
 - [ApplicationRequest](docs/ApplicationRequest.md)
 - [ApplicationRequestApplicants](docs/ApplicationRequestApplicants.md)
 - [ApplicationRequestContactDetails](docs/ApplicationRequestContactDetails.md)
 - [ApplicationRequestContactDetailsResidentialAddress](docs/ApplicationRequestContactDetailsResidentialAddress.md)
 - [BadRequestError](docs/BadRequestError.md)
 - [CustomerGetResponseBody](docs/CustomerGetResponseBody.md)
 - [CustomerOccupationRequest](docs/CustomerOccupationRequest.md)
 - [CustomerOccupationResponse](docs/CustomerOccupationResponse.md)
 - [CustomerPostResponseBody](docs/CustomerPostResponseBody.md)
 - [CustomerRequest](docs/CustomerRequest.md)
 - [CustomerSourceOfWealthRequest](docs/CustomerSourceOfWealthRequest.md)
 - [CustomerSourceOfWealthResponse](docs/CustomerSourceOfWealthResponse.md)
 - [CustomerVerificationPostResponseBody](docs/CustomerVerificationPostResponseBody.md)
 - [CustomerVerificationRequest](docs/CustomerVerificationRequest.md)
 - [CustomerVerificationRequestApplicants](docs/CustomerVerificationRequestApplicants.md)
 - [CustomerVerificationRequestContactDetails](docs/CustomerVerificationRequestContactDetails.md)
 - [CustomerVerificationRequestContactDetailsPostalAddress](docs/CustomerVerificationRequestContactDetailsPostalAddress.md)
 - [CustomerVerificationRequestDriverLicense](docs/CustomerVerificationRequestDriverLicense.md)
 - [CustomerVerificationRequestMedicareCard](docs/CustomerVerificationRequestMedicareCard.md)
 - [CustomerVerificationRequestPassport](docs/CustomerVerificationRequestPassport.md)
 - [ErrorItem](docs/ErrorItem.md)
 - [ForbiddenError](docs/ForbiddenError.md)
 - [GenerateOtpForForgottenAccessCodeCommand](docs/GenerateOtpForForgottenAccessCodeCommand.md)
 - [GenerateOtpForForgottenAccessCodeResponse](docs/GenerateOtpForForgottenAccessCodeResponse.md)
 - [GenerateOtpForForgottenPasswordCommand](docs/GenerateOtpForForgottenPasswordCommand.md)
 - [GenerateOtpForForgottenPasswordResponse](docs/GenerateOtpForForgottenPasswordResponse.md)
 - [GetCustomerPayeesResponse](docs/GetCustomerPayeesResponse.md)
 - [GetFeatures](docs/GetFeatures.md)
 - [InternalServerError](docs/InternalServerError.md)
 - [InviteResponse](docs/InviteResponse.md)
 - [LendingAddress](docs/LendingAddress.md)
 - [LendingApplicantDetails](docs/LendingApplicantDetails.md)
 - [LendingApplicationDetails](docs/LendingApplicationDetails.md)
 - [LendingApplicationOverview](docs/LendingApplicationOverview.md)
 - [LendingApplicationState](docs/LendingApplicationState.md)
 - [LendingNewApplicationCommand](docs/LendingNewApplicationCommand.md)
 - [LendingPersonalLoanProduct](docs/LendingPersonalLoanProduct.md)
 - [LendingProduct](docs/LendingProduct.md)
 - [LendingSubmitApplicationResponse](docs/LendingSubmitApplicationResponse.md)
 - [LendingUpdateApplicationDetailsCommand](docs/LendingUpdateApplicationDetailsCommand.md)
 - [LendingViewApplicationDetailsResponse](docs/LendingViewApplicationDetailsResponse.md)
 - [LendingViewApplicationsResponse](docs/LendingViewApplicationsResponse.md)
 - [NotFoundError](docs/NotFoundError.md)
 - [NppPaymentGetResponse](docs/NppPaymentGetResponse.md)
 - [NppPaymentsAccountPostRequest](docs/NppPaymentsAccountPostRequest.md)
 - [NppPaymentsAccountPostRequestOtpVerification](docs/NppPaymentsAccountPostRequestOtpVerification.md)
 - [NppPaymentsAccountPostRequestPayee](docs/NppPaymentsAccountPostRequestPayee.md)
 - [NppPaymentsAccountPostResponse](docs/NppPaymentsAccountPostResponse.md)
 - [NppPaymentsAliasPostRequest](docs/NppPaymentsAliasPostRequest.md)
 - [NppPaymentsAliasPostRequestAlias](docs/NppPaymentsAliasPostRequestAlias.md)
 - [NppPaymentsAliasPostResponse](docs/NppPaymentsAliasPostResponse.md)
 - [NppRejectedPaymentsGetResponse](docs/NppRejectedPaymentsGetResponse.md)
 - [NppResolveAccountGetResponse](docs/NppResolveAccountGetResponse.md)
 - [NppResolveAliasGetResponse](docs/NppResolveAliasGetResponse.md)
 - [OccupationCode](docs/OccupationCode.md)
 - [OccupationCodeListResponse](docs/OccupationCodeListResponse.md)
 - [Payee](docs/Payee.md)
 - [Payment](docs/Payment.md)
 - [PendingMobileNumberPostResponseBody](docs/PendingMobileNumberPostResponseBody.md)
 - [PendingMobileNumberRequest](docs/PendingMobileNumberRequest.md)
 - [PendingTransactionList](docs/PendingTransactionList.md)
 - [PendingTransactionListItem](docs/PendingTransactionListItem.md)
 - [PendingUserRequest](docs/PendingUserRequest.md)
 - [PreconditionFailedError](docs/PreconditionFailedError.md)
 - [PreconditionFailedErrorAttempts](docs/PreconditionFailedErrorAttempts.md)
 - [PreconditionFailedErrorOtpRecipients](docs/PreconditionFailedErrorOtpRecipients.md)
 - [ResetForgottenAccessCodeCommand](docs/ResetForgottenAccessCodeCommand.md)
 - [ResetForgottenAccessCodeResponse](docs/ResetForgottenAccessCodeResponse.md)
 - [ResetForgottenPasswordCommand](docs/ResetForgottenPasswordCommand.md)
 - [TermsAndConditionsPostResponseBody](docs/TermsAndConditionsPostResponseBody.md)
 - [TermsAndConditionsRequest](docs/TermsAndConditionsRequest.md)
 - [TransactionList](docs/TransactionList.md)
 - [TransactionListItem](docs/TransactionListItem.md)
 - [TransactionPage](docs/TransactionPage.md)
 - [UnauthorizedAltError](docs/UnauthorizedAltError.md)
 - [UnprocessableEntityError](docs/UnprocessableEntityError.md)
 - [ValidatedAddress](docs/ValidatedAddress.md)
 - [ValidatedAddressResidentialAddress](docs/ValidatedAddressResidentialAddress.md)
 - [VerifyMobileNumberPostResponseBody](docs/VerifyMobileNumberPostResponseBody.md)
 - [VerifyMobileNumberRequest](docs/VerifyMobileNumberRequest.md)
 - [VerifyUserPostResponseBody](docs/VerifyUserPostResponseBody.md)
 - [VerifyUserRequest](docs/VerifyUserRequest.md)


## Documentation for Authorization

All endpoints do not require authorization.
Authentication schemes defined for the API:

## Recommendation

It's recommended to create an instance of `ApiClient` per thread in a multithreaded environment to avoid any potential issues.

## Author

support@greater.com.au

