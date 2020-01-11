
# Order

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **kotlin.Long** |  |  [optional]
**petId** | **kotlin.Long** |  |  [optional]
**quantity** | **kotlin.Int** |  |  [optional]
**shipDate** | [**java.time.OffsetDateTime**](java.time.OffsetDateTime.md) |  |  [optional]
**status** | [**inline**](#StatusEnum) | Order Status |  [optional]
**complete** | **kotlin.Boolean** |  |  [optional]


<a name="StatusEnum"></a>
## Enum: status
Name | Value
---- | -----
status | placed, approved, delivered



