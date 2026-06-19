# AppleReq
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Cultivar** | **String** |  | 
**Mealy** | **Boolean** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$AppleReq = Initialize-PSPetstoreAppleReq  -Cultivar null `
 -Mealy null
```

- Convert the resource to JSON
```powershell
$AppleReq | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

