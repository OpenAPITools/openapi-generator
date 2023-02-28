# GmFruit
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Color** | **String** |  | [optional] 
**Cultivar** | **String** |  | [optional] 
**Origin** | **String** |  | [optional] 
**LengthCm** | **Decimal** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$GmFruit = Initialize-PSPetstoreGmFruit  -Color null `
 -Cultivar null `
 -Origin null `
 -LengthCm null
```

- Convert the resource to JSON
```powershell
$GmFruit | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

