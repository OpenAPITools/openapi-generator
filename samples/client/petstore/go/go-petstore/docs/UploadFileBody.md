# UploadFileBody

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | Pointer to **string** | Additional data to pass to server | [optional] 
**File** | Pointer to [***os.File**](*os.File.md) | file to upload | [optional] 

## Methods

### NewUploadFileBody

`func NewUploadFileBody() *UploadFileBody`

NewUploadFileBody instantiates a new UploadFileBody object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUploadFileBodyWithDefaults

`func NewUploadFileBodyWithDefaults() *UploadFileBody`

NewUploadFileBodyWithDefaults instantiates a new UploadFileBody object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetAdditionalMetadata

`func (o *UploadFileBody) GetAdditionalMetadata() `

GetAdditionalMetadata returns the AdditionalMetadata field if non-nil, zero value otherwise.

### GetAdditionalMetadataOk

`func (o *UploadFileBody) GetAdditionalMetadataOk() (*, bool)`

GetAdditionalMetadataOk returns a tuple with the AdditionalMetadata field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAdditionalMetadata

`func (o *UploadFileBody) SetAdditionalMetadata(v )`

SetAdditionalMetadata sets AdditionalMetadata field to given value.

### HasAdditionalMetadata

`func (o *UploadFileBody) HasAdditionalMetadata() bool`

HasAdditionalMetadata returns a boolean if a field has been set.

### GetFile

`func (o *UploadFileBody) GetFile() `

GetFile returns the File field if non-nil, zero value otherwise.

### GetFileOk

`func (o *UploadFileBody) GetFileOk() (*, bool)`

GetFileOk returns a tuple with the File field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFile

`func (o *UploadFileBody) SetFile(v )`

SetFile sets File field to given value.

### HasFile

`func (o *UploadFileBody) HasFile() bool`

HasFile returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


