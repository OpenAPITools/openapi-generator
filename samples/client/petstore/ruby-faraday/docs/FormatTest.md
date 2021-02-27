# Petstore::FormatTest

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **integer** | **Integer** |  | [optional] |
| **int32** | **Integer** |  | [optional] |
| **int64** | **Integer** |  | [optional] |
| **number** | **Float** |  |  |
| **float** | **Float** |  | [optional] |
| **double** | **Float** |  | [optional] |
| **decimal** | [**Decimal**](Decimal.md) |  | [optional] |
| **string** | **String** |  | [optional] |
| **byte** | **String** |  |  |
| **binary** | **File** |  | [optional] |
| **date** | **Date** |  |  |
| **date_time** | **Time** |  | [optional] |
| **uuid** | **String** |  | [optional] |
| **password** | **String** |  |  |
| **pattern_with_digits** | **String** | A string that is a 10 digit number. Can have leading zeros. | [optional] |
| **pattern_with_digits_and_delimiter** | **String** | A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01. | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::FormatTest.new(
  integer: null,
  int32: null,
  int64: null,
  number: null,
  float: null,
  double: null,
  decimal: null,
  string: null,
  byte: null,
  binary: null,
  date: null,
  date_time: null,
  uuid: 72f98069-206d-4f12-9f12-3d1e525a8e84,
  password: null,
  pattern_with_digits: null,
  pattern_with_digits_and_delimiter: null
)
```

