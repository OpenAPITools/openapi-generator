# EnumNullUnion

`EnumNullUnion` represents the `oneOf` union in the OpenAPI schema. A value matches one of these alternatives:

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Kind** | Pointer to **NullableString** |  | [optional] 

## Wrapping an alternative

- `NullableEnumAsEnumNullUnion(v *NullableEnum) EnumNullUnion`
- `NullableExcludedEnumAsEnumNullUnion(v *NullableExcludedEnum) EnumNullUnion`

### SetKindNil

`func (o *EnumNullUnion) SetKindNil(b bool)`

 SetKindNil sets the value for Kind to be an explicit nil

### UnsetKind
`func (o *EnumNullUnion) UnsetKind()`

UnsetKind ensures that no value is present for Kind, not even an explicit nil

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
