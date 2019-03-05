# Petstore::TypeHolderDefault

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**string_item** | **String** |  | [optional] [default to &#39;what&#39;]
**number_item** | **Float** |  | [optional] [default to 1.234]
**integer_item** | **Integer** |  | [optional] [default to -2]
**bool_item** | **BOOLEAN** |  | [optional] [default to true]
**date_item** | **Date** |  | [optional] 
**datetime_item** | **DateTime** |  | [optional] 
**array_item** | **Array&lt;Integer&gt;** |  | [optional] 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::TypeHolderDefault.new(string_item: null,
                                 number_item: null,
                                 integer_item: null,
                                 bool_item: null,
                                 date_item: null,
                                 datetime_item: null,
                                 array_item: null)
```


