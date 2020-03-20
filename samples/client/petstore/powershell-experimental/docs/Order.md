# Order
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** |  | [optional] [default to null]
**PetId** | **Int64** |  | [optional] [default to null]
**Quantity** | **Int32** |  | [optional] [default to null]
**ShipDate** | **System.DateTime** |  | [optional] [default to null]
**Status** | **String** | Order Status | [optional] [default to null]
**Complete** | **Boolean** |  | [optional] [default to false]

## Examples

- Create a new object
```powershell
New-PSPetstoreOrder  -Id null `
 -PetId null `
 -Quantity null `
 -ShipDate null `
 -Status null `
 -Complete null
```

- Convert the object to JSON
```powershell
$ | Convert-ToJSON
```


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

