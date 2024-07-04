# EnumArrays
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**JustSymbol** | **String** |  | [optional] 
**ArrayEnum** | **String[]** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$EnumArrays = Initialize-PSPetstoreEnumArrays  -JustSymbol null `
 -ArrayEnum null
```

- Convert the resource to JSON
```powershell
$EnumArrays | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

