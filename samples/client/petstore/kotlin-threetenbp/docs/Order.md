
# Order

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **kotlin.Long** |  |  [optional]
**petId** | **kotlin.Long** |  |  [optional]
**quantity** | **kotlin.Int** |  |  [optional]
**shipDate** | [**org.threeten.bp.OffsetDateTime**](org.threeten.bp.OffsetDateTime.md) |  |  [optional]
**status** | [**inline**](#Status) | Order Status |  [optional]
**complete** | **kotlin.Boolean** |  |  [optional]


<a name="Status"></a>
## Enum: status
Name | Value
---- | -----
status | placed, approved, delivered



