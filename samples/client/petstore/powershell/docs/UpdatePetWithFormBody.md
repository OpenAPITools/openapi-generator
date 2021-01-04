# UpdatePetWithFormBody
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Name** | **String** | Updated name of the pet | [optional] 
**Status** | **String** | Updated status of the pet | [optional] 

## Examples

- Prepare the resource
```powershell
$UpdatePetWithFormBody = Initialize-PSPetstoreUpdatePetWithFormBody  -Name null `
 -Status null
```

- Convert the resource to JSON
```powershell
$UpdatePetWithFormBody | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

