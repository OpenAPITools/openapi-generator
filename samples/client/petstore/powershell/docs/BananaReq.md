# BananaReq
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**LengthCm** | **Decimal** |  | 
**Sweet** | **Boolean** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$BananaReq = Initialize-PSPetstoreBananaReq  -LengthCm null `
 -Sweet null
```

- Convert the resource to JSON
```powershell
$BananaReq | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

