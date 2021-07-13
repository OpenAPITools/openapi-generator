

# NppPaymentGetResponse

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**amount** | **String** | Transfer amount | 
**completed** | **String** | (UTC) Date/time the payment was completed (Cleared or Rejected). If null, the payment is not yet complete |  [optional]
**created** | **String** | (UTC) Date/time the payment was created in the system | 
**creditor** | [**Account**](Account.md) |  |  [optional]
**creditorAlias** | [**Alias**](Alias.md) |  |  [optional]
**creditReceiptNumber** | **String** | Core receipt number to display to creditor (returned from core &#39;Credit&#39; action) |  [optional]
**debitReceiptNumber** | **String** | Core receipt number to display to debtor (returned from core &#39;Hold&#39; and/or &#39;Debit&#39; actions) |  [optional]
**debtor** | [**Account**](Account.md) |  |  [optional]
**description** | **String** | Message from debtor to creditor |  [optional]
**endToEndId** | **String** | The end-to-end id that was supplied with the payment; debtor provided such as an invoice number. |  [optional]
**hasReturns** | **Boolean** | Indicates if the payment is subject to return payments |  [optional]
**paymentId** | **String** | Unique payment identifier - UUID | 
**status** | **String** | Payment status {Pending, Failed, Queued, Initiated, Cleared, Settled, Rejected | 
**transactionId** | **String** | NPP-style transaction id |  [optional]




