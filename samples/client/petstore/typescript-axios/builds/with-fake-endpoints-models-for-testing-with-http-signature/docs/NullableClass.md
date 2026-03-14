# NullableClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**integer_prop** | **number** |  | [optional] [default to undefined]
**number_prop** | **number** |  | [optional] [default to undefined]
**boolean_prop** | **boolean** |  | [optional] [default to false]
**string_prop** | **string** |  | [optional] [default to undefined]
**date_prop** | **string** |  | [optional] [default to undefined]
**datetime_prop** | **string** |  | [optional] [default to undefined]
**array_nullable_prop** | **Array&lt;object&gt;** |  | [optional] [default to undefined]
**array_and_items_nullable_prop** | **Array&lt;object | null&gt;** |  | [optional] [default to undefined]
**array_items_nullable** | **Array&lt;object | null&gt;** |  | [optional] [default to undefined]
**object_nullable_prop** | **{ [key: string]: object; }** |  | [optional] [default to undefined]
**object_and_items_nullable_prop** | **{ [key: string]: object | null; }** |  | [optional] [default to undefined]
**object_items_nullable** | **{ [key: string]: object | null; }** |  | [optional] [default to undefined]

## Example

```typescript
import { NullableClass } from './api';

const instance: NullableClass = {
    integer_prop,
    number_prop,
    boolean_prop,
    string_prop,
    date_prop,
    datetime_prop,
    array_nullable_prop,
    array_and_items_nullable_prop,
    array_items_nullable,
    object_nullable_prop,
    object_and_items_nullable_prop,
    object_items_nullable,
};
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
