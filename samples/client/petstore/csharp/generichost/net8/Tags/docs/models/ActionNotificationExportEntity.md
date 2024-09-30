# Org.OpenAPITools.Model.ActionNotificationExportEntity
ActionNotificationExportEntity model

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**EndAt** | **DateTime** | End date/time of export range. | [optional] 
**ExportVersion** | **string** | Version of the underlying records for the export. | [optional] 
**Id** | **int** | History Export ID | [optional] 
**QueryFolder** | **string** | Return notifications that were triggered by actions in this folder. | [optional] 
**QueryMessage** | **string** | Error message associated with the request, if any. | [optional] 
**QueryPath** | **string** | Return notifications that were triggered by actions on this specific path. | [optional] 
**QueryRequestMethod** | **string** | The HTTP request method used by the webhook. | [optional] 
**QueryRequestUrl** | **string** | The target webhook URL. | [optional] 
**QueryStatus** | **string** | The HTTP status returned from the server in response to the webhook request. | [optional] 
**QuerySuccess** | **bool** | true if the webhook request succeeded (i.e. returned a 200 or 204 response status). false otherwise. | [optional] 
**ResultsUrl** | **string** | If &#x60;status&#x60; is &#x60;ready&#x60;, this will be a URL where all the results can be downloaded at once as a CSV. | [optional] 
**StartAt** | **DateTime** | Start date/time of export range. | [optional] 
**Status** | **string** | Status of export.  Valid values: &#x60;building&#x60;, &#x60;ready&#x60;, or &#x60;failed&#x60; | [optional] 

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

