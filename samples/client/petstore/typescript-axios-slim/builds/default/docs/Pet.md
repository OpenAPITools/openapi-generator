# Pet

A pet for sale in the pet store

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **number** |  | [optional] [default to undefined]
**category** | [**Category**](Category.md) |  | [optional] [default to undefined]
**name** | **string** |  | [default to undefined]
**photoUrls** | **Array&lt;string&gt;** |  | [default to undefined]
**tags** | [**Array&lt;Tag&gt;**](Tag.md) |  | [optional] [default to undefined]
**status** | **string** | pet status in the store | [optional] [default to undefined]

## Example

```typescript
import { Pet } from './api';

const instance: Pet = {
    id,
    category,
    name,
    photoUrls,
    tags,
    status,
};
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
