# petstore_api.model.enum_test.EnumTest

#### Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  |  | 
#### Dictionary Keys

Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**enum_string_required** | str,  | str,  |  |  must be one of ["UPPER", "lower", "", ]
**enum_string** | str,  | str,  |  | [optional]  must be one of ["UPPER", "lower", "", ]
**enum_integer** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional]  must be one of [1, -1, ]
**enum_number** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional]  must be one of [1.1, -1.2, ]
**stringEnum** | [**StringEnum**](StringEnum.md) | [**StringEnum**](StringEnum.md) |  | [optional]  must be one of ["placed", "approved", "delivered", "single quoted", "multiple\nlines", "double quote \n with newline", ]
**IntegerEnum** | [**IntegerEnum**](IntegerEnum.md) | [**IntegerEnum**](IntegerEnum.md) |  | [optional]  must be one of [0, 1, 2, ]
**StringEnumWithDefaultValue** | [**StringEnumWithDefaultValue**](StringEnumWithDefaultValue.md) | [**StringEnumWithDefaultValue**](StringEnumWithDefaultValue.md) |  | [optional]  must be one of ["placed", "approved", "delivered", ]
**IntegerEnumWithDefaultValue** | [**IntegerEnumWithDefaultValue**](IntegerEnumWithDefaultValue.md) | [**IntegerEnumWithDefaultValue**](IntegerEnumWithDefaultValue.md) |  | [optional]  must be one of [0, 1, 2, ]
**IntegerEnumOneValue** | [**IntegerEnumOneValue**](IntegerEnumOneValue.md) | [**IntegerEnumOneValue**](IntegerEnumOneValue.md) |  | [optional]  must be one of [0, ]
**any string name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

