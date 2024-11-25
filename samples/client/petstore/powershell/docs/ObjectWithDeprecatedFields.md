# ObjectWithDeprecatedFields
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Uuid** | **String** |  | [optional] 
**Id** | **Decimal** |  | [optional] 
**DeprecatedRef** | [**DeprecatedObject**](DeprecatedObject.md) |  | [optional] 
**Bars** | **String[]** |  | [optional] 
**SomethingElse** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$ObjectWithDeprecatedFields = Initialize-PSPetstoreObjectWithDeprecatedFields  -Uuid null `
 -Id null `
 -DeprecatedRef null `
 -Bars null `
 -SomethingElse null
```

- Convert the resource to JSON
```powershell
$ObjectWithDeprecatedFields | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

