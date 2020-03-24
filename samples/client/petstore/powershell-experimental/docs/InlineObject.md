# InlineObject
## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Name** | **String** | Updated name of the pet | [optional] [default to null]
**Status** | **String** | Updated status of the pet | [optional] [default to null]

## Examples

- Create a new object
```powershell
New-PSPetstoreInlineObject  -Name null `
 -Status null
```

- Convert the object to JSON
```powershell
$ | Convert-ToJSON
```


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

