# Animal
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ClassName** | **String** |  | 
**Color** | **String** |  | [optional] [default to "red"]

## Examples

- Prepare the resource
```powershell
$Animal = Initialize-PSPetstoreAnimal  -ClassName null `
 -Color null
```

- Convert the resource to JSON
```powershell
$Animal | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

