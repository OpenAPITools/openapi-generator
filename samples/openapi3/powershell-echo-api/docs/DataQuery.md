# DataQuery
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** | Query | [optional] 
**Outcomes** | **String[]** |  | [optional] 
**Suffix** | **String** | test suffix | [optional] 
**Text** | **String** | Some text containing white spaces | [optional] 
**Date** | **System.DateTime** | A date | [optional] 

## Examples

- Prepare the resource
```powershell
$DataQuery = Initialize-PSOpenAPIToolsDataQuery  -Id null `
 -Outcomes null `
 -Suffix null `
 -Text Some text `
 -Date null
```

- Convert the resource to JSON
```powershell
$DataQuery | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

