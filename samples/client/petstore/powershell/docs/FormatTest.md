# FormatTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Integer** | **Int32** |  | [optional] [default to null]
**Int32** | **Int32** |  | [optional] [default to null]
**Int64** | **Int64** |  | [optional] [default to null]
**Number** | **Decimal** |  | [default to null]
**Float** | **Double** |  | [optional] [default to null]
**Double** | **Double** |  | [optional] [default to null]
**String** | **String** |  | [optional] [default to null]
**Byte** | [**SystemByte**](SystemByte.md) |  | [default to null]
**Binary** | **System.IO.FileInfo** |  | [optional] [default to null]
**Date** | **System.DateTime** |  | [default to null]
**DateTime** | **System.DateTime** |  | [optional] [default to null]
**Uuid** | **String** |  | [optional] [default to null]
**Password** | **String** |  | [default to null]
**PatternWithDigits** | **String** | A string that is a 10 digit number. Can have leading zeros. | [optional] [default to null]
**PatternWithDigitsAndDelimiter** | **String** | A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01. | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreFormatTest  -Integer null `
 -Int32 null `
 -Int64 null `
 -Number null `
 -Float null `
 -Double null `
 -String null `
 -Byte null `
 -Binary null `
 -Date Sun Feb 02 00:00:00 UTC 2020 `
 -DateTime 2007-12-03T10:15:30+01:00 `
 -Uuid 72f98069-206d-4f12-9f12-3d1e525a8e84 `
 -Password null `
 -PatternWithDigits null `
 -PatternWithDigitsAndDelimiter null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

