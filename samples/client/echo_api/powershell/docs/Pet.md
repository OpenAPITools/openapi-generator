# Pet
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** |  | [optional] 
**Name** | **String** |  | 
**Category** | [**Category**](Category.md) |  | [optional] 
**PhotoUrls** | **String[]** |  | 
**Tags** | [**Tag[]**](Tag.md) |  | [optional] 
**Status** | **String** | pet status in the store | [optional] 

## Examples

- Prepare the resource
```powershell
$Pet = Initialize-PSOpenAPIToolsPet  -Id 10 `
 -Name doggie `
 -Category null `
 -PhotoUrls null `
 -Tags null `
 -Status null
```

- Convert the resource to JSON
```powershell
$Pet | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

