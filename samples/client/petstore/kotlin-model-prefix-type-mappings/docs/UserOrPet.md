
# ApiUserOrPet

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **kotlin.String** |  | 
**photoUrls** | **kotlin.collections.List&lt;kotlin.String&gt;** |  | 
**id** | **kotlin.Long** |  |  [optional]
**username** | **kotlin.String** |  |  [optional]
**firstName** | **kotlin.String** |  |  [optional]
**lastName** | **kotlin.String** |  |  [optional]
**email** | **kotlin.String** |  |  [optional]
**password** | **kotlin.String** |  |  [optional]
**phone** | **kotlin.String** |  |  [optional]
**userStatus** | **kotlin.Int** | User Status |  [optional]
**category** | [**ApiCategory**](ApiCategory.md) |  |  [optional]
**tags** | [**kotlin.collections.List&lt;ApiTag&gt;**](ApiTag.md) |  |  [optional]
**status** | [**inline**](#Status) | pet status in the store |  [optional]


<a id="Status"></a>
## Enum: status
Name | Value
---- | -----
status | available, pending, sold



