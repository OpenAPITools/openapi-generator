# ApiResponse
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Code** | **Int32** |  | [optional] 
**Type** | **String** |  | [optional] 
**Message** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$ApiResponse = Initialize-PSPetstoreApiResponse  -Code null `
 -Type null `
 -Message null
```

- Convert the resource to JSON
```powershell
$ApiResponse | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

