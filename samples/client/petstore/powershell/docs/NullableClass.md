# NullableClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**IntegerProp** | **Int32** |  | [optional] 
**NumberProp** | **Decimal** |  | [optional] 
**BooleanProp** | **Boolean** |  | [optional] 
**StringProp** | **String** |  | [optional] 
**DateProp** | **System.DateTime** |  | [optional] 
**DatetimeProp** | **System.DateTime** |  | [optional] 
**ArrayNullableProp** | [**SystemCollectionsHashtable[]**](SystemCollectionsHashtable.md) |  | [optional] 
**ArrayAndItemsNullableProp** | [**SystemCollectionsHashtable[]**](SystemCollectionsHashtable.md) |  | [optional] 
**ArrayItemsNullable** | [**SystemCollectionsHashtable[]**](SystemCollectionsHashtable.md) |  | [optional] 
**ObjectNullableProp** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] 
**ObjectAndItemsNullableProp** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] 
**ObjectItemsNullable** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] 

## Examples

- Prepare the resource
```powershell
$NullableClass = Initialize-PSPetstoreNullableClass  -IntegerProp null `
 -NumberProp null `
 -BooleanProp null `
 -StringProp null `
 -DateProp null `
 -DatetimeProp null `
 -ArrayNullableProp null `
 -ArrayAndItemsNullableProp null `
 -ArrayItemsNullable null `
 -ObjectNullableProp null `
 -ObjectAndItemsNullableProp null `
 -ObjectItemsNullable null
```

- Convert the resource to JSON
```powershell
$NullableClass | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

