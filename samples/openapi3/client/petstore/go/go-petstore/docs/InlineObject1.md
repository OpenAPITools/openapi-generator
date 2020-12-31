# InlineObject1

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | Pointer to **string** | Additional data to pass to server | [optional] 
**File** | Pointer to ***os.File** | file to upload | [optional] 

## Methods

### NewInlineObject1

`func NewInlineObject1() *InlineObject1`

NewInlineObject1 instantiates a new InlineObject1 object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewInlineObject1WithDefaults

`func NewInlineObject1WithDefaults() *InlineObject1`

NewInlineObject1WithDefaults instantiates a new InlineObject1 object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetAdditionalMetadata

`func (o *InlineObject1) GetAdditionalMetadata() string`

GetAdditionalMetadata returns the AdditionalMetadata field if non-nil, zero value otherwise.

### GetAdditionalMetadataOk

`func (o *InlineObject1) GetAdditionalMetadataOk() (*string, bool)`

GetAdditionalMetadataOk returns a tuple with the AdditionalMetadata field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAdditionalMetadata

`func (o *InlineObject1) SetAdditionalMetadata(v string)`

SetAdditionalMetadata sets AdditionalMetadata field to given value.

### HasAdditionalMetadata

`func (o *InlineObject1) HasAdditionalMetadata() bool`

HasAdditionalMetadata returns a boolean if a field has been set.

### GetFile

`func (o *InlineObject1) GetFile() *os.File`

GetFile returns the File field if non-nil, zero value otherwise.

### GetFileOk

`func (o *InlineObject1) GetFileOk() (**os.File, bool)`

GetFileOk returns a tuple with the File field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFile

`func (o *InlineObject1) SetFile(v *os.File)`

SetFile sets File field to given value.

### HasFile

`func (o *InlineObject1) HasFile() bool`

HasFile returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


