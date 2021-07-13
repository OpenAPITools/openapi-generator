

# PendingTransactionListItem

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**accountId** | **String** | The id of the account the transaction is for | 
**description** | **String** | The description of the transaction | 
**merchantName** | **String** | The merchant Name of the transaction |  [optional]
**availabilityDate** | [**Date**](Date.md) | The date-time the funds will become available (the hold will be released). |  [optional]
**amount** | [**BigDecimal**](BigDecimal.md) | The amount of the transaction | 
**uncollectedFundsType** | [**UncollectedFundsTypeEnum**](#UncollectedFundsTypeEnum) | Indicates the type of uncollected funds. | 


## Enum: UncollectedFundsTypeEnum

Name | Value
---- | -----




