# petstore_api.model.nullable_shape.NullableShape

The value may be a shape or the 'null' value. For a composed schema to validate a null payload, one of its chosen oneOf schemas must be type null or nullable (introduced in OAS schema >= 3.0)

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO | The value may be a shape or the &#x27;null&#x27; value. For a composed schema to validate a null payload, one of its chosen oneOf schemas must be type null or nullable (introduced in OAS schema &gt;&#x3D; 3.0) | 

### Composed Schemas (allOf/anyOf/oneOf/not)
#### oneOf
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
[Triangle](Triangle.md) | [**Triangle**](Triangle.md) | [**Triangle**](Triangle.md) |  | 
[Quadrilateral](Quadrilateral.md) | [**Quadrilateral**](Quadrilateral.md) | [**Quadrilateral**](Quadrilateral.md) |  | 
[one_of_2](#one_of_2) | None,  | NoneClass,  |  | 

# one_of_2

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
None,  | NoneClass,  |  | 

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

