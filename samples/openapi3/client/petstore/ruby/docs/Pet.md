# Petstore::Pet

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **Integer** |  | [optional] 
**category** | [**Category**](Category.md) |  | [optional] 
**name** | **String** |  | 
**photo_urls** | **Array&lt;String&gt;** |  | 
**tags** | [**Array&lt;Tag&gt;**](Tag.md) |  | [optional] 
**status** | **String** | pet status in the store | [optional] 

## Code Sample

```ruby
require 'Petstore'

instance = Petstore::Pet.new(id: null,
                                 category: null,
                                 name: doggie,
                                 photo_urls: null,
                                 tags: null,
                                 status: null)
```


