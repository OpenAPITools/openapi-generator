# AdditionalPropertiesClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MapProperty** | **System.Collections.Hashtable** |  | [optional] [default to null]
**MapOfMapProperty** | [**System.Collections.Hashtable**](Map.md) |  | [optional] [default to null]
**Anytype1** | [**AnyType**](.md) |  | [optional] [default to null]
**MapWithUndeclaredPropertiesAnytype1** | [**SystemCollectionsHashtable**](.md) |  | [optional] [default to null]
**MapWithUndeclaredPropertiesAnytype2** | [**SystemCollectionsHashtable**](.md) |  | [optional] [default to null]
**MapWithUndeclaredPropertiesAnytype3** | [**System.Collections.Hashtable**](SystemCollectionsHashtable.md) |  | [optional] [default to null]
**EmptyMap** | [**SystemCollectionsHashtable**](.md) | an object with no declared properties and no undeclared properties, hence it&#39;s an empty map. | [optional] [default to null]
**MapWithUndeclaredPropertiesString** | **System.Collections.Hashtable** |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreAdditionalPropertiesClass  -MapProperty null `
 -MapOfMapProperty null `
 -Anytype1 null `
 -MapWithUndeclaredPropertiesAnytype1 null `
 -MapWithUndeclaredPropertiesAnytype2 null `
 -MapWithUndeclaredPropertiesAnytype3 null `
 -EmptyMap null `
 -MapWithUndeclaredPropertiesString null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

