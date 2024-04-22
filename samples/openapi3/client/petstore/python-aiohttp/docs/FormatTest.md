# FormatTest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**integer** | **int** |  | [optional] 
**int32** | **int** |  | [optional] 
**int64** | **int** |  | [optional] 
**number** | **float** |  | 
**var_float** | **float** |  | [optional] 
**double** | **float** |  | [optional] 
**decimal** | **decimal.Decimal** |  | [optional] 
**string** | **str** |  | [optional] 
**string_with_double_quote_pattern** | **str** |  | [optional] 
**byte** | **bytearray** |  | [optional] 
**binary** | **bytearray** |  | [optional] 
**var_date** | **date** |  | 
**date_time** | **datetime** |  | [optional] 
**uuid** | **str** |  | [optional] 
**password** | **str** |  | 
**pattern_with_digits** | **str** | A string that is a 10 digit number. Can have leading zeros. | [optional] 
**pattern_with_digits_and_delimiter** | **str** | A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01. | [optional] 

## Example

```python
from petstore_api.models.format_test import FormatTest

# TODO update the JSON string below
json = "{}"
# create an instance of FormatTest from a JSON string
format_test_instance = FormatTest.from_json(json)
# print the JSON string representation of the object
print(FormatTest.to_json())

# convert the object into a dict
format_test_dict = format_test_instance.to_dict()
# create an instance of FormatTest from a dict
format_test_from_dict = FormatTest.from_dict(format_test_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


