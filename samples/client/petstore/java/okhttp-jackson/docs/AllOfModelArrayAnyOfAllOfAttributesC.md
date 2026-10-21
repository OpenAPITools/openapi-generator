

# AllOfModelArrayAnyOfAllOfAttributesC


## Properties

| Name | Type | Description | Notes |
|------------ | ------------- | ------------- | -------------|
|**id** | **Long** |  |  [optional] |
|**category** | [**Category**](Category.md) |  |  [optional] |
|**name** | **String** |  |  |
|**photoUrls** | **List&lt;String&gt;** |  |  |
|**tags** | [**List&lt;Tag&gt;**](Tag.md) |  |  [optional] |
|**status** | [**StatusEnum**](#StatusEnum) | Order Status |  [optional] |
|**petId** | **Long** |  |  [optional] |
|**quantity** | **Integer** |  |  [optional] |
|**shipDate** | **OffsetDateTime** |  |  [optional] |
|**complete** | **Boolean** |  |  [optional] |



## Enum: StatusEnum

| Name | Value |
|---- | -----|
| PLACED | &quot;placed&quot; |
| APPROVED | &quot;approved&quot; |
| DELIVERED | &quot;delivered&quot; |



