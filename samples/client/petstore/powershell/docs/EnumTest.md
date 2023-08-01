# EnumTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**EnumString** | **String** |  | [optional] 
**EnumStringRequired** | **String** |  | 
**EnumInteger** | **Int32** |  | [optional] 
**EnumIntegerOnly** | **Int32** |  | [optional] 
**EnumNumber** | **Double** |  | [optional] 
**OuterEnum** | [**OuterEnum**](OuterEnum.md) |  | [optional] 

## Examples

- Prepare the resource
```powershell
$EnumTest = Initialize-PSPetstoreEnumTest  -EnumString null `
 -EnumStringRequired null `
 -EnumInteger null `
 -EnumIntegerOnly null `
 -EnumNumber null `
 -OuterEnum null
```

- Convert the resource to JSON
```powershell
$EnumTest | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

