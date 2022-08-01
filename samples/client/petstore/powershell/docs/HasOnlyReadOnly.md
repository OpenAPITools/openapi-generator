# HasOnlyReadOnly
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Bar** | **String** |  | [optional] [readonly] 
**Foo** | **String** |  | [optional] [readonly] 

## Examples

- Prepare the resource
```powershell
$HasOnlyReadOnly = Initialize-PSPetstoreHasOnlyReadOnly  -Bar null `
 -Foo null
```

- Convert the resource to JSON
```powershell
$HasOnlyReadOnly | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

