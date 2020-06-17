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
**ObjectWithNoDeclaredProps** | [**SystemCollectionsHashtable**](.md) | test code generation for objects Value must be a map of strings to values. It cannot be the &#39;null&#39; value. | [optional] [default to null]
**ObjectWithNoDeclaredPropsNullable** | [**SystemCollectionsHashtable**](.md) | test code generation for nullable objects. Value must be a map of strings to values or the &#39;null&#39; value. | [optional] [default to null]
**AnyTypeProp** | [**AnyType**](.md) | test code generation for any type Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389 | [optional] [default to null]
**AnyTypePropNullable** | [**AnyType**](.md) | test code generation for any type Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The &#39;nullable&#39; attribute does not change the allowed values. | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreUser  -Id null `
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
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

