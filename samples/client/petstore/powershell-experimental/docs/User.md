# User
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **Int64** |  | [optional] [default to null]
**Username** | **String** |  | [optional] [default to null]
**FirstName** | **String** |  | [optional] [default to null]
**LastName** | **String** |  | [optional] [default to null]
**Email** | **String** |  | [optional] [default to null]
**Password** | **String** |  | [optional] [default to null]
**Phone** | **String** |  | [optional] [default to null]
**UserStatus** | **Int32** | User Status | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Prepare-PSPetstoreUser  -Id null `
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
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

