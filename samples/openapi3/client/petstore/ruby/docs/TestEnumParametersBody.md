# Petstore::TestEnumParametersBody

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**enum_form_string_array** | **Array&lt;String&gt;** | Form parameter enum test (string array) | [optional] 
**enum_form_string** | **String** | Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::TestEnumParametersBody.new(enum_form_string_array: null,
                                 enum_form_string: null)
```


