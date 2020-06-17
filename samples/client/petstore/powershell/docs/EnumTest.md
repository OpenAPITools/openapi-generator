# EnumTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**EnumString** | **String** |  | [optional] [default to null]
**EnumStringRequired** | **String** |  | [default to null]
**EnumInteger** | **Int32** |  | [optional] [default to null]
**EnumNumber** | **Double** |  | [optional] [default to null]
**OuterEnum** | [**OuterEnum**](OuterEnum.md) |  | [optional] [default to null]
**OuterEnumInteger** | [**OuterEnumInteger**](OuterEnumInteger.md) |  | [optional] [default to null]
**OuterEnumDefaultValue** | [**OuterEnumDefaultValue**](OuterEnumDefaultValue.md) |  | [optional] [default to null]
**OuterEnumIntegerDefaultValue** | [**OuterEnumIntegerDefaultValue**](OuterEnumIntegerDefaultValue.md) |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreEnumTest  -EnumString null `
 -EnumStringRequired null `
 -EnumInteger null `
 -EnumNumber null `
 -OuterEnum null `
 -OuterEnumInteger null `
 -OuterEnumDefaultValue null `
 -OuterEnumIntegerDefaultValue null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

