# Name
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Name** | **Int32** |  | 
**SnakeCase** | **Int32** |  | [optional] [readonly] 
**Property** | **String** |  | [optional] 
**Var123Number** | **Int32** |  | [optional] [readonly] 

## Examples

- Prepare the resource
```powershell
$Name = Initialize-PSPetstoreName  -Name null `
 -SnakeCase null `
 -Property null `
 -Var123Number null
```

- Convert the resource to JSON
```powershell
$Name | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

