# Order

An order for a pets from the pet store

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **number** |  | [optional] [default to undefined]
**petId** | **number** |  | [optional] [default to undefined]
**quantity** | **number** |  | [optional] [default to undefined]
**shipDate** | **string** |  | [optional] [default to undefined]
**stringWithAttemptedInjection** | **string** | This is an example of a string property that includes attempted injection attack content. It should be properly escaped and handled by the server to prevent security vulnerabilities. ${attemptedStringInter}\\backslash\&quot;\&quot;\&quot;attemptToBreakOutOfMultiline | [optional] [default to undefined]
**status** | **string** | Order Status | [optional] [default to undefined]
**complete** | **boolean** |  | [optional] [default to false]

## Example

```typescript
import { Order } from '@openapitools/typescript-axios-petstore';

const instance: Order = {
    id,
    petId,
    quantity,
    shipDate,
    stringWithAttemptedInjection,
    status,
    complete,
};
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
