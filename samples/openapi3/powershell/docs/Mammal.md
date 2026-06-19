# Mammal
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**HasBaleen** | **Boolean** |  | [optional] 
**HasTeeth** | **Boolean** |  | [optional] 
**ClassName** | **String** |  | 
**Type** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$Mammal = Initialize-PSPetstoreMammal  -HasBaleen null `
 -HasTeeth null `
 -ClassName null `
 -Type null
```

- Convert the resource to JSON
```powershell
$Mammal | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

