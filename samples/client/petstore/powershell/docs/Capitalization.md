# Capitalization
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**SmallCamel** | **String** |  | [optional] 
**CapitalCamel** | **String** |  | [optional] 
**SmallSnake** | **String** |  | [optional] 
**CapitalSnake** | **String** |  | [optional] 
**SCAETHFlowPoints** | **String** |  | [optional] 
**ATTNAME** | **String** | Name of the pet  | [optional] 

## Examples

- Prepare the resource
```powershell
$Capitalization = Initialize-PSPetstoreCapitalization  -SmallCamel null `
 -CapitalCamel null `
 -SmallSnake null `
 -CapitalSnake null `
 -SCAETHFlowPoints null `
 -ATTNAME null
```

- Convert the resource to JSON
```powershell
$Capitalization | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

