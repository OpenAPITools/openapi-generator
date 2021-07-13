

# TransactionListItem

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **String** | The unique identifier of the transaction | 
**accountId** | **String** | The id of the account the transaction is for | 
**description** | **String** | The description of the transaction | 
**category** | **String** | The category of the transaction | 
**executionDateTime** | [**Date**](Date.md) | The date the transaction was executed by the originating customer | 
**postingDateTime** | [**Date**](Date.md) | The time the transaction was posted | 
**amount** | [**BigDecimal**](BigDecimal.md) | The amount of the transaction | 
**balanceAmount** | [**BigDecimal**](BigDecimal.md) | The running balance | 
**creditDebitIndicator** | [**CreditDebitIndicatorEnum**](#CreditDebitIndicatorEnum) | Indicates whether the transaction is credit or debit |  [optional]
**nppPaymentId** | **String** | The NPP payment ID (if the transaction is an NPP payment) |  [optional]


## Enum: CreditDebitIndicatorEnum

Name | Value
---- | -----




