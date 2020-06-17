# InlineObject3
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Integer** | **Int32** | None | [optional] [default to null]
**Int32** | **Int32** | None | [optional] [default to null]
**Int64** | **Int64** | None | [optional] [default to null]
**Number** | **Decimal** | None | [default to null]
**Float** | **Double** | None | [optional] [default to null]
**Double** | **Double** | None | [default to null]
**String** | **String** | None | [optional] [default to null]
**PatternWithoutDelimiter** | **String** | None | [default to null]
**Byte** | [**SystemByte**](SystemByte.md) | None | [default to null]
**Binary** | **System.IO.FileInfo** | None | [optional] [default to null]
**Date** | **System.DateTime** | None | [optional] [default to null]
**DateTime** | **System.DateTime** | None | [optional] [default to 2010-02-01T10:20:10.111110+01:00]
**Password** | **String** | None | [optional] [default to null]
**Callback** | **String** | None | [optional] [default to null]

## Examples

- Prepare the resource
```powershell
Initialize-PSPetstoreInlineObject3  -Integer null `
 -Int32 null `
 -Int64 null `
 -Number null `
 -Float null `
 -Double null `
 -String null `
 -PatternWithoutDelimiter null `
 -Byte null `
 -Binary null `
 -Date null `
 -DateTime 2020-02-02T20:20:20.222220Z `
 -Password null `
 -Callback null
```

- Convert the resource to JSON
```powershell
$ | Convert-ToJSON
```

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

