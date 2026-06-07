
# Order

## Properties
| Name | Type | Description | Notes |
| ------------ | ------------- | ------------- | ------------- |
| **id** | **kotlin.Long** |  |  [optional] |
| **petId** | **kotlin.Long** |  |  [optional] |
| **quantity** | **kotlin.Int** |  |  [optional] |
| **shipDate** | **kotlin.String** |  |  [optional] |
| **stringWithAttemptedInjection** | **kotlin.String** | This is an example of a string property that includes attempted injection attack content. It should be properly escaped and handled by the server to prevent security vulnerabilities. ${attemptedStringInter}\\backslash\&quot;\&quot;\&quot;attemptToBreakOutOfMultiline |  [optional] |
| **status** | [**inline**](#Status) | Order Status |  [optional] |
| **complete** | **kotlin.Boolean** |  |  [optional] |


<a id="Status"></a>
## Enum: status
| Name | Value |
| ---- | ----- |
| status | placed, approved, delivered |



