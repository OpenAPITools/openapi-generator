
# Table `Order`
(mapped from: Order)

## Properties
Name | Mapping | SQL Type | Default | Type | Description | Notes
---- | ------- | -------- | ------- | ---- | ----------- | -----
**id** | id | long PRIMARY KEY AUTOINCREMENT |  | **kotlin.Long** |  |  [optional]
**petId** | petId | long |  | **kotlin.Long** |  |  [optional]
**quantity** | quantity | int |  | **kotlin.Int** |  |  [optional]
**shipDate** | shipDate | datetime |  | [**java.time.LocalDateTime**](java.time.LocalDateTime.md) |  |  [optional]
**status** | status | text |  | [**status**](#Status) | Order Status |  [optional]
**complete** | complete | boolean |  | **kotlin.Boolean** |  |  [optional]








