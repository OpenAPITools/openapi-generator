# FruitReq
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Cultivar** | **String** |  | 
**Mealy** | **Boolean** |  | [optional] 
**LengthCm** | **Decimal** |  | 
**Sweet** | **Boolean** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$FruitReq = Initialize-PSPetstoreFruitReq  -Cultivar null `
 -Mealy null `
 -LengthCm null `
 -Sweet null
```

- Convert the resource to JSON
```powershell
$FruitReq | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

