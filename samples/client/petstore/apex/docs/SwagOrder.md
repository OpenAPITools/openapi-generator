
# SwagOrder

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **Long** |  |  [optional]
**petId** | **Long** |  |  [optional]
**quantity** | **Integer** |  |  [optional]
**shipDate** | **Datetime** |  |  [optional]
**status** | [**StatusEnum**](#StatusEnum) | Order Status |  [optional]
**complete** | **Boolean** |  |  [optional]


<a name="StatusEnum"></a>
## Enum: StatusEnum
Name | Value
---- | -----
PLACED | &quot;placed&quot;
APPROVED | &quot;approved&quot;
DELIVERED | &quot;delivered&quot;



