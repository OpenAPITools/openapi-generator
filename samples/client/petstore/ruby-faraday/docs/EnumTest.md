# Petstore::EnumTest

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**enum_string** | **String** |  | [optional] 
**enum_string_required** | **String** |  | 
**enum_integer** | **Integer** |  | [optional] 
**enum_number** | **Float** |  | [optional] 
**outer_enum** | [**OuterEnum**](OuterEnum.md) |  | [optional] 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::EnumTest.new(enum_string: null,
                                 enum_string_required: null,
                                 enum_integer: null,
                                 enum_number: null,
                                 outer_enum: null)
```


