# Drawing
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**MainShape** | [**Shape**](Shape.md) |  | [optional] [default to null]
**ShapeOrNull** | [**ShapeOrNull**](ShapeOrNull.md) |  | [optional] [default to null]
**NullableShape** | [**NullableShape**](NullableShape.md) |  | [optional] [default to null]
**Shapes** | [**Shape[]**](Shape.md) |  | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreDrawing  -MainShape null `
 -ShapeOrNull null `
 -NullableShape null `
 -Shapes null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

