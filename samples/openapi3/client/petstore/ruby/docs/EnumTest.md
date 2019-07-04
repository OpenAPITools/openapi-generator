# Petstore::EnumTest

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**enum_string** | **String** |  | [optional] 
**enum_string_required** | **String** |  | 
**enum_integer** | **Integer** |  | [optional] 
**enum_number** | **Float** |  | [optional] 
**outer_enum** | [**OuterEnum**](OuterEnum.md) |  | [optional] 
**outer_enum_integer** | [**OuterEnumInteger**](OuterEnumInteger.md) |  | [optional] 
**outer_enum_default_value** | [**OuterEnumDefaultValue**](OuterEnumDefaultValue.md) |  | [optional] 
**outer_enum_integer_default_value** | [**OuterEnumIntegerDefaultValue**](OuterEnumIntegerDefaultValue.md) |  | [optional] 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::EnumTest.new(enum_string: null,
                                 enum_string_required: null,
                                 enum_integer: null,
                                 enum_number: null,
                                 outer_enum: null,
                                 outer_enum_integer: null,
                                 outer_enum_default_value: null,
                                 outer_enum_integer_default_value: null)
```


