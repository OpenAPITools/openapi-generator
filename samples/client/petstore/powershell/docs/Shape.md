# Shape
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ShapeType** | **String** |  | 
**TriangleType** | **String** |  | 
**QuadrilateralType** | **String** |  | 

## Examples

- Prepare the resource
```powershell
$Shape = Initialize-PSPetstoreShape  -ShapeType null `
 -TriangleType null `
 -QuadrilateralType null
```

- Convert the resource to JSON
```powershell
$Shape | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

