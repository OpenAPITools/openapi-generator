# Petstore::InlineObject3

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**integer** | **Integer** | None | [optional] 
**int32** | **Integer** | None | [optional] 
**int64** | **Integer** | None | [optional] 
**number** | **Float** | None | 
**float** | **Float** | None | [optional] 
**double** | **Float** | None | 
**string** | **String** | None | [optional] 
**pattern_without_delimiter** | **String** | None | 
**byte** | **String** | None | 
**binary** | **File** | None | [optional] 
**date** | **Date** | None | [optional] [default to Date.parse(&quot;2010-02-01&quot;)]
**date_time** | **DateTime** | None | [optional] [default to DateTime.parse(&quot;2010-02-01T09:20:10.111110Z[UTC]&quot;)]
**password** | **String** | None | [optional] 
**callback** | **String** | None | [optional] 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::InlineObject3.new(integer: null,
                                 int32: null,
                                 int64: null,
                                 number: null,
                                 float: null,
                                 double: null,
                                 string: null,
                                 pattern_without_delimiter: null,
                                 byte: null,
                                 binary: null,
                                 date: null,
                                 date_time: null,
                                 password: null,
                                 callback: null)
```


