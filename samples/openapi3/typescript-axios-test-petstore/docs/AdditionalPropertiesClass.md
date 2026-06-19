# AdditionalPropertiesClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**map_property** | **{ [key: string]: string; }** |  | [optional] [default to undefined]
**map_of_map_property** | **{ [key: string]: { [key: string]: string; }; }** |  | [optional] [default to undefined]
**anytype_1** | **any** |  | [optional] [default to undefined]
**map_with_undeclared_properties_anytype_1** | **object** |  | [optional] [default to undefined]
**map_with_undeclared_properties_anytype_2** | **object** |  | [optional] [default to undefined]
**map_with_undeclared_properties_anytype_3** | **{ [key: string]: any; }** |  | [optional] [default to undefined]
**empty_map** | **object** | an object with no declared properties and no undeclared properties, hence it\&#39;s an empty map. | [optional] [default to undefined]
**map_with_undeclared_properties_string** | **{ [key: string]: string; }** |  | [optional] [default to undefined]

## Example

```typescript
import { AdditionalPropertiesClass } from './api';

const instance: AdditionalPropertiesClass = {
    map_property,
    map_of_map_property,
    anytype_1,
    map_with_undeclared_properties_anytype_1,
    map_with_undeclared_properties_anytype_2,
    map_with_undeclared_properties_anytype_3,
    empty_map,
    map_with_undeclared_properties_string,
};
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
