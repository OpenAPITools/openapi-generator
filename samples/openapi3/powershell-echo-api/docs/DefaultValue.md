# DefaultValue
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ArrayStringEnumRefDefault** | [**StringEnumRef[]**](StringEnumRef.md) |  | [optional] 
**ArrayStringEnumDefault** | **String[]** |  | [optional] 
**ArrayStringDefault** | **String[]** |  | [optional] 
**ArrayIntegerDefault** | **Int32[]** |  | [optional] 
**ArrayString** | **String[]** |  | [optional] 
**ArrayStringNullable** | **String[]** |  | [optional] 
**ArrayStringExtensionNullable** | **String[]** |  | [optional] 
**StringNullable** | **String** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$DefaultValue = Initialize-PSOpenAPIToolsDefaultValue  -ArrayStringEnumRefDefault null `
 -ArrayStringEnumDefault null `
 -ArrayStringDefault null `
 -ArrayIntegerDefault null `
 -ArrayString null `
 -ArrayStringNullable null `
 -ArrayStringExtensionNullable null `
 -StringNullable null
```

- Convert the resource to JSON
```powershell
$DefaultValue | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

