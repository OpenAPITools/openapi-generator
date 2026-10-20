# User


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **number** |  | [optional] [default to undefined]
**username** | **string** |  | [optional] [default to undefined]
**firstName** | **string** |  | [optional] [default to undefined]
**lastName** | **string** |  | [optional] [default to undefined]
**email** | **string** |  | [optional] [default to undefined]
**password** | **string** |  | [optional] [default to undefined]
**phone** | **string** |  | [optional] [default to undefined]
**userStatus** | **number** | User Status | [optional] [default to undefined]
**objectWithNoDeclaredProps** | **object** | test code generation for objects Value must be a map of strings to values. It cannot be the \&#39;null\&#39; value. | [optional] [default to undefined]
**objectWithNoDeclaredPropsNullable** | **object** | test code generation for nullable objects. Value must be a map of strings to values or the \&#39;null\&#39; value. | [optional] [default to undefined]
**anyTypeProp** | **any** | test code generation for any type Here the \&#39;type\&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389 | [optional] [default to undefined]
**anyTypePropNullable** | **any** | test code generation for any type Here the \&#39;type\&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The \&#39;nullable\&#39; attribute does not change the allowed values. | [optional] [default to undefined]

## Example

```typescript
import { User } from './api';

const instance: User = {
    id,
    username,
    firstName,
    lastName,
    email,
    password,
    phone,
    userStatus,
    objectWithNoDeclaredProps,
    objectWithNoDeclaredPropsNullable,
    anyTypeProp,
    anyTypePropNullable,
};
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
