# ArrayTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ArrayOfString** | **String[]** |  | [optional] 
**ArrayArrayOfInteger** | [**Int64[][]**](Array.md) |  | [optional] 
**ArrayArrayOfModel** | [**ReadOnlyFirst[][]**](Array.md) |  | [optional] 

## Examples

- Prepare the resource
```powershell
$ArrayTest = Initialize-PSPetstoreArrayTest  -ArrayOfString null `
 -ArrayArrayOfInteger null `
 -ArrayArrayOfModel null
```

- Convert the resource to JSON
```powershell
$ArrayTest | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

