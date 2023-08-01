# OuterComposite
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MyNumber** | **Decimal** |  | [optional] 
**MyString** | **String** |  | [optional] 
**MyBoolean** | **Boolean** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$OuterComposite = Initialize-PSPetstoreOuterComposite  -MyNumber null `
 -MyString null `
 -MyBoolean null
```

- Convert the resource to JSON
```powershell
$OuterComposite | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

