# UploadFileWithRequiredFileBody

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | Pointer to **string** | Additional data to pass to server | [optional] 
**RequiredFile** | [***os.File**](*os.File.md) | file to upload | 

## Methods

### NewUploadFileWithRequiredFileBody

`func NewUploadFileWithRequiredFileBody(RequiredFile *os.File, ) *UploadFileWithRequiredFileBody`

NewUploadFileWithRequiredFileBody instantiates a new UploadFileWithRequiredFileBody object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUploadFileWithRequiredFileBodyWithDefaults

`func NewUploadFileWithRequiredFileBodyWithDefaults() *UploadFileWithRequiredFileBody`

NewUploadFileWithRequiredFileBodyWithDefaults instantiates a new UploadFileWithRequiredFileBody object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetAdditionalMetadata

`func (o *UploadFileWithRequiredFileBody) GetAdditionalMetadata() `

GetAdditionalMetadata returns the AdditionalMetadata field if non-nil, zero value otherwise.

### GetAdditionalMetadataOk

`func (o *UploadFileWithRequiredFileBody) GetAdditionalMetadataOk() (*, bool)`

GetAdditionalMetadataOk returns a tuple with the AdditionalMetadata field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAdditionalMetadata

`func (o *UploadFileWithRequiredFileBody) SetAdditionalMetadata(v )`

SetAdditionalMetadata sets AdditionalMetadata field to given value.

### HasAdditionalMetadata

`func (o *UploadFileWithRequiredFileBody) HasAdditionalMetadata() bool`

HasAdditionalMetadata returns a boolean if a field has been set.

### GetRequiredFile

`func (o *UploadFileWithRequiredFileBody) GetRequiredFile() `

GetRequiredFile returns the RequiredFile field if non-nil, zero value otherwise.

### GetRequiredFileOk

`func (o *UploadFileWithRequiredFileBody) GetRequiredFileOk() (*, bool)`

GetRequiredFileOk returns a tuple with the RequiredFile field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetRequiredFile

`func (o *UploadFileWithRequiredFileBody) SetRequiredFile(v )`

SetRequiredFile sets RequiredFile field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


