

# Pet


## Properties

| Name | Type | Description | Notes |
|------------ | ------------- | ------------- | -------------|
|**id** | **Long** |  |  [optional] |
|**name** | **String** |  |  |
|**category** | [**Category**](Category.md) |  |  [optional] |
|**photoUrls** | **List&lt;String&gt;** |  |  |
|**tags** | [**List&lt;Tag&gt;**](Tag.md) |  |  [optional] |
|**status** | [**StatusEnum**](#StatusEnum) | pet status in the store |  [optional] |



## Enum: StatusEnum

| Name | Value |
|---- | -----|
| AVAILABLE | &quot;available&quot; |
| PENDING | &quot;pending&quot; |
| SOLD | &quot;sold&quot; |



