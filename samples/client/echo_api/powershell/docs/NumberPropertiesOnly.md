# NumberPropertiesOnly
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Number** | **Decimal** |  | [optional] 
**Float** | **Double** |  | [optional] 
**Double** | **Double** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$NumberPropertiesOnly = Initialize-PSOpenAPIToolsNumberPropertiesOnly  -Number null `
 -Float null `
 -Double null
```

- Convert the resource to JSON
```powershell
$NumberPropertiesOnly | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

