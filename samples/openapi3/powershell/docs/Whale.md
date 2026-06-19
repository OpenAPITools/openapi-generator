# Whale
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**HasBaleen** | **Boolean** |  | [optional] 
**HasTeeth** | **Boolean** |  | [optional] 
**ClassName** | **String** |  | 

## Examples

- Prepare the resource
```powershell
$Whale = Initialize-PSPetstoreWhale  -HasBaleen null `
 -HasTeeth null `
 -ClassName null
```

- Convert the resource to JSON
```powershell
$Whale | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

