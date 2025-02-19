# Query
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** | Query | [optional] 
**Outcomes** | **String[]** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$Query = Initialize-PSOpenAPIToolsQuery  -Id null `
 -Outcomes null
```

- Convert the resource to JSON
```powershell
$Query | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

