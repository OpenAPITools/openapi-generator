# Order
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** |  | [optional] 
**PetId** | **Int64** |  | [optional] 
**Quantity** | **Int32** |  | [optional] 
**ShipDate** | **System.DateTime** |  | [optional] 
**Status** | **String** | Order Status | [optional] 
**Complete** | **Boolean** |  | [optional] [default to $false]

## Examples

- Prepare the resource
```powershell
$Order = Initialize-PSPetstoreOrder  -Id null `
 -PetId null `
 -Quantity null `
 -ShipDate null `
 -Status null `
 -Complete null
```

- Convert the resource to JSON
```powershell
$Order | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

