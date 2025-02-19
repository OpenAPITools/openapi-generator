# FileSchemaTestClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**File** | [**File**](File.md) |  | [optional] 
**Files** | [**File[]**](File.md) |  | [optional] 

## Examples

- Prepare the resource
```powershell
$FileSchemaTestClass = Initialize-PSPetstoreFileSchemaTestClass  -File null `
 -Files null
```

- Convert the resource to JSON
```powershell
$FileSchemaTestClass | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

