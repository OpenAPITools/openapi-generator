# AdditionalPropertiesClass
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MapProperty** | **System.Collections.Hashtable** |  | [optional] 
**MapOfMapProperty** | [**System.Collections.Hashtable**](Map.md) |  | [optional] 
**Anytype1** | [**AnyType**](.md) |  | [optional] 
**MapWithUndeclaredPropertiesAnytype1** | [**SystemCollectionsHashtable**](.md) |  | [optional] 
**MapWithUndeclaredPropertiesAnytype2** | [**SystemCollectionsHashtable**](.md) |  | [optional] 
**MapWithUndeclaredPropertiesAnytype3** | [**System.Collections.Hashtable**](AnyType.md) |  | [optional] 
**EmptyMap** | [**SystemCollectionsHashtable**](.md) | an object with no declared properties and no undeclared properties, hence it&#39;s an empty map. | [optional] 
**MapWithUndeclaredPropertiesString** | **System.Collections.Hashtable** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$AdditionalPropertiesClass = Initialize-PSPetstoreAdditionalPropertiesClass  -MapProperty null `
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
$AdditionalPropertiesClass | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

