# Mammal
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**HasBaleen** | **Boolean** |  | [optional] [default to null]
**HasTeeth** | **Boolean** |  | [optional] [default to null]
**ClassName** | **String** |  | [default to null]
**Type** | **String** |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreMammal  -HasBaleen null `
 -HasTeeth null `
 -ClassName null `
 -Type null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

