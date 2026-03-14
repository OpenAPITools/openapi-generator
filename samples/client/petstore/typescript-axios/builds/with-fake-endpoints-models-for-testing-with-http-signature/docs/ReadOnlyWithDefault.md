# ReadOnlyWithDefault


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**prop1** | **string** |  | [optional] [readonly] [default to undefined]
**prop2** | **string** |  | [optional] [readonly] [default to 'defaultProp2']
**prop3** | **string** |  | [optional] [default to 'defaultProp3']
**boolProp1** | **boolean** |  | [optional] [readonly] [default to false]
**boolProp2** | **boolean** |  | [optional] [default to true]
**intProp1** | **number** |  | [optional] [readonly] [default to 100]
**intProp2** | **number** |  | [optional] [default to 120]

## Example

```typescript
import { ReadOnlyWithDefault } from './api';

const instance: ReadOnlyWithDefault = {
    prop1,
    prop2,
    prop3,
    boolProp1,
    boolProp2,
    intProp1,
    intProp2,
};
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
