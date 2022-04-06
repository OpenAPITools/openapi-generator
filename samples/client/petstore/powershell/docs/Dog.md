# Dog
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ClassName** | **String** |  | 
**Color** | **String** |  | [optional] [default to "red"]
**Breed** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$Dog = Initialize-PSPetstoreDog  -ClassName null `
 -Color null `
 -Breed null
```

- Convert the resource to JSON
```powershell
$Dog | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

