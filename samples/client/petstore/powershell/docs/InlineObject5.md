# InlineObject5
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | **String** | Additional data to pass to server | [optional] [default to null]
**RequiredFile** | **System.IO.FileInfo** | file to upload | [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreInlineObject5  -AdditionalMetadata null `
 -RequiredFile null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

