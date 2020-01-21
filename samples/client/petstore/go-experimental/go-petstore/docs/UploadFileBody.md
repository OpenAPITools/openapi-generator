# UploadFileBody

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | Pointer to **string** | Additional data to pass to server | [optional] 
**File** | Pointer to [***os.File**](*os.File.md) | file to upload | [optional] 

## Methods

### GetAdditionalMetadata

`func (o *UploadFileBody) GetAdditionalMetadata() string`

GetAdditionalMetadata returns the AdditionalMetadata field if non-nil, zero value otherwise.

### GetAdditionalMetadataOk

`func (o *UploadFileBody) GetAdditionalMetadataOk() (string, bool)`

GetAdditionalMetadataOk returns a tuple with the AdditionalMetadata field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasAdditionalMetadata

`func (o *UploadFileBody) HasAdditionalMetadata() bool`

HasAdditionalMetadata returns a boolean if a field has been set.

### SetAdditionalMetadata

`func (o *UploadFileBody) SetAdditionalMetadata(v string)`

SetAdditionalMetadata gets a reference to the given string and assigns it to the AdditionalMetadata field.

### GetFile

`func (o *UploadFileBody) GetFile() *os.File`

GetFile returns the File field if non-nil, zero value otherwise.

### GetFileOk

`func (o *UploadFileBody) GetFileOk() (*os.File, bool)`

GetFileOk returns a tuple with the File field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasFile

`func (o *UploadFileBody) HasFile() bool`

HasFile returns a boolean if a field has been set.

### SetFile

`func (o *UploadFileBody) SetFile(v *os.File)`

SetFile gets a reference to the given *os.File and assigns it to the File field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


