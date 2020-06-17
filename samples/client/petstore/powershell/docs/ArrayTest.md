# ArrayTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ArrayOfString** | **String[]** |  | [optional] [default to null]
**ArrayArrayOfInteger** | [**Int64[][]**](Array.md) |  | [optional] [default to null]
**ArrayArrayOfModel** | [**ReadOnlyFirst[][]**](Array.md) |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreArrayTest  -ArrayOfString null `
 -ArrayArrayOfInteger null `
 -ArrayArrayOfModel null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

