# ReadOnlyFirst
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Bar** | **String** |  | [optional] [readonly] 
**Baz** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$ReadOnlyFirst = Initialize-PSPetstoreReadOnlyFirst  -Bar null `
 -Baz null
```

- Convert the resource to JSON
```powershell
$ReadOnlyFirst | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

