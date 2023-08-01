# Apple
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Cultivar** | **String** |  | [optional] 
**Origin** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$Apple = Initialize-PSPetstoreApple  -Cultivar null `
 -Origin null
```

- Convert the resource to JSON
```powershell
$Apple | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

