
# Table `Pet`
(mapped from: Pet)

## Properties
Name | Mapping | SQL Type | Default | Type | Description | Notes
---- | ------- | -------- | ------- | ---- | ----------- | -----
**name** | name | text NOT NULL |  | **kotlin.String** |  | 
**photoUrls** | `One-To-Many` | `----` | `----`  | **kotlin.Array&lt;kotlin.String&gt;** |  | 
**id** | id | long PRIMARY KEY AUTOINCREMENT |  | **kotlin.Long** |  |  [optional]
**category** | category | long |  | [**Category**](Category.md) |  |  [optional] [foreignkey]
**tags** | `One-To-Many` | `----` | `----`  | [**kotlin.Array&lt;Tag&gt;**](Tag.md) |  |  [optional]
**status** | status | text |  | [**status**](#Status) | pet status in the store |  [optional]



# **Table `PetPhotoUrls`**
(mapped from: PetPhotoUrls)

## Properties
Name | Mapping | SQL Type | Default | Type | Description | Notes
---- | ------- | -------- | ------- | ---- | ----------- | -----
pet | pet | long | | kotlin.Long | Primary Key | *one*
photoUrls | photoUrls | text | | kotlin.String | Foreign Key | *many*





# **Table `PetTag`**
(mapped from: PetTag)

## Properties
Name | Mapping | SQL Type | Default | Type | Description | Notes
---- | ------- | -------- | ------- | ---- | ----------- | -----
pet | pet | long | | kotlin.Long | Primary Key | *one*
tag | tag | long | | kotlin.Long | Foreign Key | *many*




