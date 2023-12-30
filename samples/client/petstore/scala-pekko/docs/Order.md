

# Order

An order for a pets from the pet store

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **Long** |  |  [optional]
**petId** | **Long** |  |  [optional]
**quantity** | **Int** |  |  [optional]
**shipDate** | **OffsetDateTime** |  |  [optional]
**status** | [**Status**](#Status) | Order Status |  [optional]
**complete** | **Boolean** |  |  [optional]


## Enum: Status
Allowed values: [placed, approved, delivered]




