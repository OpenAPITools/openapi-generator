# MapTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MapMapOfString** | [**System.Collections.Hashtable**](Map.md) |  | [optional] 
**MapOfEnumString** | **System.Collections.Hashtable** |  | [optional] 
**DirectMap** | **System.Collections.Hashtable** |  | [optional] 
**IndirectMap** | **System.Collections.Hashtable** |  | [optional] 

## Examples

- Prepare the resource
```powershell
$MapTest = Initialize-PSPetstoreMapTest  -MapMapOfString null `
 -MapOfEnumString null `
 -DirectMap null `
 -IndirectMap null
```

- Convert the resource to JSON
```powershell
$MapTest | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

