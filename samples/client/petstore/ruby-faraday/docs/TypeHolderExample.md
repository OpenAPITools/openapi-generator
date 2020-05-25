# Petstore::TypeHolderExample

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**string_item** | **String** |  | 
**number_item** | **Float** |  | 
**float_item** | **Float** |  | 
**integer_item** | **Integer** |  | 
**bool_item** | **Boolean** |  | 
**array_item** | **Array&lt;Integer&gt;** |  | 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::TypeHolderExample.new(string_item: what,
                                 number_item: 1.234,
                                 float_item: 1.234,
                                 integer_item: -2,
                                 bool_item: true,
                                 array_item: [0, 1, 2, 3])
```


