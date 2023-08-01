# FormatTest
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Integer** | **Int32** |  | [optional] 
**Int32** | **Int32** |  | [optional] 
**Int64** | **Int64** |  | [optional] 
**Number** | **Decimal** |  | 
**Float** | **Double** |  | [optional] 
**Double** | **Double** |  | [optional] 
**Decimal** | **Decimal** |  | [optional] 
**String** | **String** |  | [optional] 
**Byte** | [**SystemByte**](SystemByte.md) |  | 
**Binary** | **System.IO.FileInfo** |  | [optional] 
**Date** | **System.DateTime** |  | 
**DateTime** | **System.DateTime** |  | [optional] 
**Uuid** | **String** |  | [optional] 
**Password** | **String** |  | 
**PatternWithDigits** | **String** | A string that is a 10 digit number. Can have leading zeros. | [optional] 
**PatternWithDigitsAndDelimiter** | **String** | A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01. | [optional] 

## Examples

- Prepare the resource
```powershell
$FormatTest = Initialize-PSPetstoreFormatTest  -Integer null `
 -Int32 null `
 -Int64 null `
 -Number null `
 -Float null `
 -Double null `
 -Decimal null `
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
$FormatTest | ConvertTo-JSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

