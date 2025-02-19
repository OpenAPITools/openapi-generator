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
**ObjectWithNoDeclaredProps** | [**SystemCollectionsHashtable**](.md) | test code generation for objects Value must be a map of strings to values. It cannot be the &#39;null&#39; value. | [optional] 
**ObjectWithNoDeclaredPropsNullable** | [**SystemCollectionsHashtable**](.md) | test code generation for nullable objects. Value must be a map of strings to values or the &#39;null&#39; value. | [optional] 
**AnyTypeProp** | [**AnyType**](.md) | test code generation for any type Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389 | [optional] 
**AnyTypePropNullable** | [**AnyType**](.md) | test code generation for any type Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The &#39;nullable&#39; attribute does not change the allowed values. | [optional] 

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
 -UserStatus null `
 -ObjectWithNoDeclaredProps null `
 -ObjectWithNoDeclaredPropsNullable null `
 -AnyTypeProp null `
 -AnyTypePropNullable null
```

- Convert the resource to JSON
```powershell
$User | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

