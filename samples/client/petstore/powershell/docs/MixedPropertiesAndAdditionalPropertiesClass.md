# MixedPropertiesAndAdditionalPropertiesClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Uuid** | **String** |  | [optional] [default to null]
**DateTime** | **System.DateTime** |  | [optional] [default to null]
**Map** | [**System.Collections.Hashtable**](Animal.md) |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreMixedPropertiesAndAdditionalPropertiesClass  -Uuid null `
 -DateTime null `
 -Map null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

