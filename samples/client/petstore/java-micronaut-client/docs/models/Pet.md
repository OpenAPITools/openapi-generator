

# Pet

The class is defined in **[Pet.java](../../src/main/java/org/openapitools/model/Pet.java)**

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | `Long` |  |  [optional property]
**category** | [`Category`](Category.md) |  |  [optional property]
**name** | `String` |  | 
**photoUrls** | `Set&lt;String&gt;` |  | 
**tags** | [`List&lt;Tag&gt;`](Tag.md) |  |  [optional property]
**status** | [**StatusEnum**](#StatusEnum) | pet status in the store |  [optional property]






## StatusEnum

Name | Value
---- | -----
AVAILABLE | `"available"`
PENDING | `"pending"`
SOLD | `"sold"`


