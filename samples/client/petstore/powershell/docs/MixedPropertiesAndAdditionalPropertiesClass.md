# MixedPropertiesAndAdditionalPropertiesClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Uuid** | **String** |  | [optional] 
**DateTime** | **System.DateTime** |  | [optional] 
**Map** | [**System.Collections.Hashtable**](Animal.md) |  | [optional] 

## Examples

- Prepare the resource
```powershell
$MixedPropertiesAndAdditionalPropertiesClass = Initialize-PSPetstoreMixedPropertiesAndAdditionalPropertiesClass  -Uuid null `
 -DateTime null `
 -Map null
```

- Convert the resource to JSON
```powershell
$MixedPropertiesAndAdditionalPropertiesClass | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

