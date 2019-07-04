# Petstore::TypeHolderDefault

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**string_item** | **String** |  | [default to &#39;what&#39;]
**number_item** | **Float** |  | 
**integer_item** | **Integer** |  | 
**bool_item** | **Boolean** |  | [default to true]
**array_item** | **Array&lt;Integer&gt;** |  | 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::TypeHolderDefault.new(string_item: null,
                                 number_item: null,
                                 integer_item: null,
                                 bool_item: null,
                                 array_item: null)
```


