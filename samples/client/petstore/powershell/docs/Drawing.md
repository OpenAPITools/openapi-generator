# Drawing
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MainShape** | [**Shape**](Shape.md) |  | [optional] 
**ShapeOrNull** | [**ShapeOrNull**](ShapeOrNull.md) |  | [optional] 
**NullableShape** | [**NullableShape**](NullableShape.md) |  | [optional] 
**Shapes** | [**Shape[]**](Shape.md) |  | [optional] 

## Examples

- Prepare the resource
```powershell
$Drawing = Initialize-PSPetstoreDrawing  -MainShape null `
 -ShapeOrNull null `
 -NullableShape null `
 -Shapes null
```

- Convert the resource to JSON
```powershell
$Drawing | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

