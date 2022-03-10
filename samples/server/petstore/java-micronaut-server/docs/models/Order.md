

# Order

An order for a pets from the pet store

The class is defined in **[Order.java](../../src/main/java/org/openapitools/model/Order.java)**

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | `Long` |  |  [optional property]
**petId** | `Long` |  |  [optional property]
**quantity** | `Integer` |  |  [optional property]
**shipDate** | `LocalDateTime` |  |  [optional property]
**status** | [**StatusEnum**](#StatusEnum) | Order Status |  [optional property]
**complete** | `Boolean` |  |  [optional property]





## StatusEnum

Name | Value
---- | -----
PLACED | `"placed"`
APPROVED | `"approved"`
DELIVERED | `"delivered"`



