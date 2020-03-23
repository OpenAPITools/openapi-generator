# InlineObject1
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | **String** | Additional data to pass to server | [optional] [default to null]
**File** | **System.IO.FileInfo** | file to upload | [optional] [default to null]

## Examples

- Create a new object
```powershell
New-PSPetstoreInlineObject1  -AdditionalMetadata null `
 -File null
```

- Convert the object to JSON
```powershell
$ | Convert-ToJSON
```


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

