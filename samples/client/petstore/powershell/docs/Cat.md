# Cat
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ClassName** | **String** |  | 
**Color** | **String** |  | [optional] [default to "red"]
**Declawed** | **Boolean** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$Cat = Initialize-PSPetstoreCat  -ClassName null `
 -Color null `
 -Declawed null
```

- Convert the resource to JSON
```powershell
$Cat | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

