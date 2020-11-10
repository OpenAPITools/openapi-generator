
# Table `Pet` 
(mapped from: Pet)

## Properties
Name | Mapping | SQL Type | Default | Type | Description | Notes
---- | ------- | -------- | ------- | ---- | ----------- | -----
**name** | name | text NOT NULL |  | **kotlin.String** |  | 
**photoUrls** | photoUrls | long NOT NULL |  | **kotlin.Array&lt;kotlin.String&gt;** |  |  [foreignkey]
**id** | id | long |  | **kotlin.Long** |  |  [optional]
**category** | category | long |  | [**Category**](Category.md) |  |  [optional] [foreignkey]
**tags** | tags | long |  | [**kotlin.Array&lt;Tag&gt;**](Tag.md) |  |  [optional] [foreignkey]
**status** | status | text |  | [**status**](#StatusEnum) | pet status in the store |  [optional]








