# User
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** |  | [optional] 
**Username** | **String** |  | [optional] 
**FirstName** | **String** |  | [optional] 
**LastName** | **String** |  | [optional] 
**Email** | **String** |  | [optional] 
**Password** | **String** |  | [optional] 
**Phone** | **String** |  | [optional] 
**UserStatus** | **Int32** | User Status | [optional] 

## Examples

- Prepare the resource
```powershell
$User = Initialize-PSPetstoreUser  -Id null `
 -Username null `
 -FirstName null `
 -LastName null `
 -Email null `
 -Password null `
 -Phone null `
 -UserStatus null
```

- Convert the resource to JSON
```powershell
$User | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

