# InlineObject1
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | **String** | Additional data to pass to server | [optional] 
**File** | **System.IO.FileInfo** | file to upload | [optional] 

## Examples

- Prepare the resource
```powershell
$InlineObject1 = Initialize-PSPetstoreInlineObject1  -AdditionalMetadata null `
 -File null
```

- Convert the resource to JSON
```powershell
$InlineObject1 | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

