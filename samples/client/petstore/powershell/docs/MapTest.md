# MapTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MapMapOfString** | [**System.Collections.Hashtable**](Map.md) |  | [optional] [default to null]
**MapOfEnumString** | **System.Collections.Hashtable** |  | [optional] [default to null]
**DirectMap** | **System.Collections.Hashtable** |  | [optional] [default to null]
**IndirectMap** | **System.Collections.Hashtable** |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreMapTest  -MapMapOfString null `
 -MapOfEnumString null `
 -DirectMap null `
 -IndirectMap null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

