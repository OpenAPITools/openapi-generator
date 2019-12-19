
# Order

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **Long** |  |  [optional]
**petId** | **Long** |  |  [optional]
**quantity** | **Int** |  |  [optional]
**shipDate** | [**java.time.LocalDateTime**](java.time.LocalDateTime.md) |  |  [optional]
**status** | [**inline**](#StatusEnum) | Order Status |  [optional]
**complete** | **Boolean** |  |  [optional]


<a name="StatusEnum"></a>
## Enum: status
Name | Value
---- | -----
status | placed, approved, delivered



