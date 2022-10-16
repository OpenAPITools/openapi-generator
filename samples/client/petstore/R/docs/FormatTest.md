# petstore::FormatTest


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**integer** | **integer** |  | [optional] [Max: 100] [Min: 10] 
**int32** | **integer** |  | [optional] [Max: 200] [Min: 20] 
**int64** | **integer** |  | [optional] 
**number** | **numeric** |  | [Max: 543.2] [Min: 32.1] 
**float** | **numeric** |  | [optional] [Max: 987.6] [Min: 54.3] 
**double** | **numeric** |  | [optional] [Max: 123.4] [Min: 67.8] 
**string** | **character** |  | [optional] [Pattern: [a-z]/i] 
**byte** | **character** |  | 
**binary** | **data.frame** |  | [optional] 
**date** | **character** |  | [default to &quot;Fri Jul 19 00:00:00 UTC 2019&quot;] 
**dateTime** | **character** |  | [optional] [default to &quot;2015-10-28T14:38:02Z&quot;] 
**uuid** | **character** |  | [optional] 
**password** | **character** |  | [Max. length: 64] [Min. length: 10] 
**pattern_with_digits** | **character** | A string that is a 10 digit number. Can have leading zeros. | [optional] [Pattern: ^\\d{10}$] 
**pattern_with_digits_and_delimiter** | **character** | A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01. | [optional] [Pattern: ^image_\\d{1,3}$/i] 


