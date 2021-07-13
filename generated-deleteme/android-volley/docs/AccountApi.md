# AccountApi

All URIs are relative to *https://sim.nonprod.gbcloud.com.au/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteV1AccountInviteInviteCode**](AccountApi.md#deleteV1AccountInviteInviteCode) | **DELETE** /v1/account-invite/{inviteCode} | Delete an account invite
[**getV1AccountsAccountIdPendingTransactions**](AccountApi.md#getV1AccountsAccountIdPendingTransactions) | **GET** /v1/accounts/{accountId}/pending-transactions | A list of pending transactions
[**getV1InviteCode**](AccountApi.md#getV1InviteCode) | **GET** /v1/account-invite/{inviteCode} | GET request for joint account invitation
[**getV2PendingTransactions**](AccountApi.md#getV2PendingTransactions) | **GET** /v2/accounts/{accountId}/pending-transactions | A list of pending transactions filtered by description (if provided)
[**getV3Transactions**](AccountApi.md#getV3Transactions) | **GET** /v3/accounts/{accountId}/transactions | Get a list transactions
[**getV3TransactionsPaged**](AccountApi.md#getV3TransactionsPaged) | **GET** /v3/accounts/{accountId}/transactions/paginated | Get a list transactions, returned as paged results.
[**postV1AccountInviteRegenerate**](AccountApi.md#postV1AccountInviteRegenerate) | **POST** /v1/account-invite/{inviteId}/regenerate | Regenerate an invite code for an existing invite
[**postV1InviteCodeAccept**](AccountApi.md#postV1InviteCodeAccept) | **POST** /v1/account-invite/{inviteId}/accept | 
[**postV3AccountsInvite**](AccountApi.md#postV3AccountsInvite) | **POST** /v3/accounts/{accountId}/invite | Invite to a joint account



## deleteV1AccountInviteInviteCode

> deleteV1AccountInviteInviteCode(inviteCode)

Delete an account invite

Delete an account invite

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String inviteCode = null; // String | 
try {
    apiInstance.deleteV1AccountInviteInviteCode(inviteCode);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#deleteV1AccountInviteInviteCode");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inviteCode** | **String**|  | [default to null]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV1AccountsAccountIdPendingTransactions

> PendingTransactionList getV1AccountsAccountIdPendingTransactions(accountId)

A list of pending transactions

Returns all pending (aka on-hold or authorisation) transactions for a given account.

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String accountId = SAV-100002612; // String | The account Id is in the form <account_type>-<account_number>.
try {
    PendingTransactionList result = apiInstance.getV1AccountsAccountIdPendingTransactions(accountId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#getV1AccountsAccountIdPendingTransactions");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountId** | **String**| The account Id is in the form &lt;account_type&gt;-&lt;account_number&gt;. | [default to null]

### Return type

[**PendingTransactionList**](PendingTransactionList.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV1InviteCode

> InviteResponse getV1InviteCode(inviteCode)

GET request for joint account invitation

Get joint account invitation 

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String inviteCode = null; // String | 
try {
    InviteResponse result = apiInstance.getV1InviteCode(inviteCode);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#getV1InviteCode");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inviteCode** | **String**|  | [default to null]

### Return type

[**InviteResponse**](InviteResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV2PendingTransactions

> PendingTransactionList getV2PendingTransactions(accountId, descriptionFilter, minimumAmount, maximumAmount)

A list of pending transactions filtered by description (if provided)

Returns all pending (aka on-hold or authorisation) transactions for a given account. The transactons can be filtered based on the query parameters.

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String accountId = null; // String | 
String descriptionFilter = null; // String | Only return transactions who's description contains this text.  The text is URL encoded.
BigDecimal minimumAmount = null; // BigDecimal | The desired amount, or low amount for a range inquiry.
BigDecimal maximumAmount = null; // BigDecimal | The high amount for a range inquiry.
try {
    PendingTransactionList result = apiInstance.getV2PendingTransactions(accountId, descriptionFilter, minimumAmount, maximumAmount);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#getV2PendingTransactions");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountId** | **String**|  | [default to null]
 **descriptionFilter** | **String**| Only return transactions who&#39;s description contains this text.  The text is URL encoded. | [optional] [default to null]
 **minimumAmount** | **BigDecimal**| The desired amount, or low amount for a range inquiry. | [optional] [default to null]
 **maximumAmount** | **BigDecimal**| The high amount for a range inquiry. | [optional] [default to null]

### Return type

[**PendingTransactionList**](PendingTransactionList.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV3Transactions

> TransactionList getV3Transactions(accountId, descriptionFilter, startDate, endDate, minimumAmount, maximumAmount, transactionType, orderDescending)

Get a list transactions

Get a list transactions. The transactions can be filtered based on the query parameters.

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String accountId = SAV-100002612; // String | The account Id is in the form <account_type>-<account_number>.
String descriptionFilter = null; // String | Only return transactions who's description contains this text.  The text is URL encoded.
Date startDate = Sun Jan 01 11:00:00 AEDT 2017; // Date | The low date selection for a date range inquiry (inclusive).
Date endDate = Fri Mar 31 11:00:00 AEDT 2017; // Date | The high date selection for a date range inquiry
Double minimumAmount = null; // Double | The desired amount, or low amount for a range inquiry.
Double maximumAmount = null; // Double | The high amount for a range inquiry.
String transactionType = null; // String | The transaction type to filter on.
Boolean orderDescending = null; // Boolean | Transactions returned ordered by newest execution to oldest execution.  Defaults to true.
try {
    TransactionList result = apiInstance.getV3Transactions(accountId, descriptionFilter, startDate, endDate, minimumAmount, maximumAmount, transactionType, orderDescending);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#getV3Transactions");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountId** | **String**| The account Id is in the form &lt;account_type&gt;-&lt;account_number&gt;. | [default to null]
 **descriptionFilter** | **String**| Only return transactions who&#39;s description contains this text.  The text is URL encoded. | [optional] [default to null]
 **startDate** | **Date**| The low date selection for a date range inquiry (inclusive). | [optional] [default to null]
 **endDate** | **Date**| The high date selection for a date range inquiry | [optional] [default to null]
 **minimumAmount** | **Double**| The desired amount, or low amount for a range inquiry. | [optional] [default to null]
 **maximumAmount** | **Double**| The high amount for a range inquiry. | [optional] [default to null]
 **transactionType** | **String**| The transaction type to filter on. | [optional] [default to null] [enum: All, Credits, Debits, Cheques, Deposits, Transfers, Atm, PointOfSale, ChargesAndFees, DirectDeposits, DirectDebits, InterestOnly, BookedPassbookOnly]
 **orderDescending** | **Boolean**| Transactions returned ordered by newest execution to oldest execution.  Defaults to true. | [optional] [default to null]

### Return type

[**TransactionList**](TransactionList.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## getV3TransactionsPaged

> TransactionPage getV3TransactionsPaged(accountId, pageNumber, pageSize, previousRequestId, startDate, endDate, minimumAmount, maximumAmount, transactionType, orderDescending)

Get a list transactions, returned as paged results.

Get a paginated list of transactions. The transactions can be filtered based on the query parameters. When getting paged results, pass the pageSize, pageNumber, previousRequestId (unless it&#39;s the first page). Also, the other filters (startDate, endDate, minimumAmount, maximumAmount, transactionType) can be used with paging searches; but if used on the first page, they must be submitted for each page request. The descriptionFilter cannot be used in conjunction with paging requests.

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String accountId = SAV-100002612; // String | The account Id is in the form <account_type>-<account_number>.
Integer pageNumber = null; // Integer | The page number to start listing transactions from (starting from page 1)
Integer pageSize = null; // Integer | The number of transactions to include in the listing (not completely respected)
String previousRequestId = null; // String | The request id of the first page request (if not the first request)
Date startDate = Sun Jan 01 11:00:00 AEDT 2017; // Date | The low date selection for a date range inquiry (inclusive).
Date endDate = Fri Mar 31 11:00:00 AEDT 2017; // Date | The high date selection for a date range inquiry
Double minimumAmount = null; // Double | The desired amount, or low amount for a range inquiry.
Double maximumAmount = null; // Double | The high amount for a range inquiry.
String transactionType = null; // String | The transaction type to filter on.
Boolean orderDescending = null; // Boolean | Transactions returned ordered by newest execution to oldest execution.  Defaults to true.
try {
    TransactionPage result = apiInstance.getV3TransactionsPaged(accountId, pageNumber, pageSize, previousRequestId, startDate, endDate, minimumAmount, maximumAmount, transactionType, orderDescending);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#getV3TransactionsPaged");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountId** | **String**| The account Id is in the form &lt;account_type&gt;-&lt;account_number&gt;. | [default to null]
 **pageNumber** | **Integer**| The page number to start listing transactions from (starting from page 1) | [optional] [default to null]
 **pageSize** | **Integer**| The number of transactions to include in the listing (not completely respected) | [optional] [default to null]
 **previousRequestId** | **String**| The request id of the first page request (if not the first request) | [optional] [default to null]
 **startDate** | **Date**| The low date selection for a date range inquiry (inclusive). | [optional] [default to null]
 **endDate** | **Date**| The high date selection for a date range inquiry | [optional] [default to null]
 **minimumAmount** | **Double**| The desired amount, or low amount for a range inquiry. | [optional] [default to null]
 **maximumAmount** | **Double**| The high amount for a range inquiry. | [optional] [default to null]
 **transactionType** | **String**| The transaction type to filter on. | [optional] [default to null] [enum: All, Credits, Debits, Cheques, Deposits, Transfers, Atm, PointOfSale, ChargesAndFees, DirectDeposits, DirectDebits, InterestOnly, BookedPassbookOnly]
 **orderDescending** | **Boolean**| Transactions returned ordered by newest execution to oldest execution.  Defaults to true. | [optional] [default to null]

### Return type

[**TransactionPage**](TransactionPage.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## postV1AccountInviteRegenerate

> AccountInviteResponse postV1AccountInviteRegenerate(inviteId)

Regenerate an invite code for an existing invite

Regenerate an invite for an existing invite

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String inviteId = null; // String | 
try {
    AccountInviteResponse result = apiInstance.postV1AccountInviteRegenerate(inviteId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#postV1AccountInviteRegenerate");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inviteId** | **String**|  | [default to null]

### Return type

[**AccountInviteResponse**](AccountInviteResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## postV1InviteCodeAccept

> postV1InviteCodeAccept(inviteId, acceptInviteRequest)



Request to accept an invite

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String inviteId = null; // String | 
AcceptInviteRequest acceptInviteRequest = new AcceptInviteRequest(); // AcceptInviteRequest | 
try {
    apiInstance.postV1InviteCodeAccept(inviteId, acceptInviteRequest);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#postV1InviteCodeAccept");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inviteId** | **String**|  | [default to null]
 **acceptInviteRequest** | [**AcceptInviteRequest**](AcceptInviteRequest.md)|  | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## postV3AccountsInvite

> AccountInviteResponse postV3AccountsInvite(accountId, accountInvite)

Invite to a joint account

Create an invite for a joint account

### Example

```java
// Import classes:
//import org.openapitools.client.api.AccountApi;

AccountApi apiInstance = new AccountApi();
String accountId = null; // String | 
AccountInvite accountInvite = new AccountInvite(); // AccountInvite | 
try {
    AccountInviteResponse result = apiInstance.postV3AccountsInvite(accountId, accountInvite);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling AccountApi#postV3AccountsInvite");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **accountId** | **String**|  | [default to null]
 **accountInvite** | [**AccountInvite**](AccountInvite.md)|  | [optional]

### Return type

[**AccountInviteResponse**](AccountInviteResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

