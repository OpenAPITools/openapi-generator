# .Order

## Properties

|Name | Type | Description | Notes|
|------------ | ------------- | ------------- | -------------|
|**id** | **number** |  | [optional] [default to &quot;-1&quot;]|
|**petId** | **number** |  | [optional] [default to 0]|
|**quantity** | **number** |  | [optional] [default to 0]|
|**shipDate** | **Date** |  | [optional] [default to undefined]|
|**status** | **string** | Order Status | [optional] [default to OrderStatusEnum.Approved]|
|**complete** | **boolean** |  | [optional] [default to false]|


## Enum: OrderStatusEnum


* `Placed` (value: `'placed'`)

* `Approved` (value: `'approved'`)

* `Delivered` (value: `'delivered'`)




