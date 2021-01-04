# UploadFileBody
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | **String** | Additional data to pass to server | [optional] 
**File** | **System.IO.FileInfo** | file to upload | [optional] 

## Examples

- Prepare the resource
```powershell
$UploadFileBody = Initialize-PSPetstoreUploadFileBody  -AdditionalMetadata null `
 -File null
```

- Convert the resource to JSON
```powershell
$UploadFileBody | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

