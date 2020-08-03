

# Order

An order for a pets from the pet store
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **Long** |  |  [optional]
**petId** | **Long** |  |  [optional]
**quantity** | **Integer** |  |  [optional]
**shipDate** | [**Date**](Date.md) |  |  [optional]
**status** | [**StatusEnum**](#StatusEnum) | Order Status |  [optional]
**complete** | **Boolean** |  |  [optional]



## Enum: StatusEnum

Name | Value
---- | -----
PLACED | "placed"
APPROVED | "approved"
DELIVERED | "delivered"



