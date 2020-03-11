# Pet
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** |  | [optional] [default to null]
**Category** | [**Category**](Category.md) |  | [optional] [default to null]
**Name** | **String** |  | [default to null]
**PhotoUrls** | **String[]** |  | [default to null]
**Tags** | [**Tag[]**](Tag.md) |  | [optional] [default to null]
**Status** | **String** | pet status in the store | [optional] [default to null]

## Examples

- Create a new object
```powershell
New-PSOpenAPIToolsPet  -Id null `
 -Category null `
 -Name doggie `
 -PhotoUrls null `
 -Tags null `
 -Status null
```

- Convert the object to JSON
```powershell
$ | Convert-ToJSON
```


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

