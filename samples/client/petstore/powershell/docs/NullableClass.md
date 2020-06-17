# NullableClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**IntegerProp** | **Int32** |  | [optional] [default to null]
**NumberProp** | **Decimal** |  | [optional] [default to null]
**BooleanProp** | **Boolean** |  | [optional] [default to null]
**StringProp** | **String** |  | [optional] [default to null]
**DateProp** | **System.DateTime** |  | [optional] [default to null]
**DatetimeProp** | **System.DateTime** |  | [optional] [default to null]
**ArrayNullableProp** | [**SystemCollectionsHashtable[]**](SystemCollectionsHashtable.md) |  | [optional] [default to null]
**ArrayAndItemsNullableProp** | [**SystemCollectionsHashtable[]**](SystemCollectionsHashtable.md) |  | [optional] [default to null]
**ArrayItemsNullable** | [**SystemCollectionsHashtable[]**](SystemCollectionsHashtable.md) |  | [optional] [default to null]
**ObjectNullableProp** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] [default to null]
**ObjectAndItemsNullableProp** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] [default to null]
**ObjectItemsNullable** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreNullableClass  -IntegerProp null `
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
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

