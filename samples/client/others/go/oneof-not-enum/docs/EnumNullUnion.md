# EnumNullUnion

`EnumNullUnion` represents the `oneOf` union in the OpenAPI schema. A value matches one of these alternatives:

- [`NullableEnum`](NullableEnum.md)
- [`NullableExcludedEnum`](NullableExcludedEnum.md)

## Wrapping an alternative

- `NullableEnumAsEnumNullUnion(v *NullableEnum) EnumNullUnion`
- `NullableExcludedEnumAsEnumNullUnion(v *NullableExcludedEnum) EnumNullUnion`

Use `GetActualInstance()` or `GetActualInstanceValue()` to access the wrapped value. `Kind` belongs to the selected alternative; `EnumNullUnion` itself has no `Kind` field or `SetKind*` methods.

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)
