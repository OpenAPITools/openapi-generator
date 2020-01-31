# InlineObject5

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**AdditionalMetadata** | Pointer to **string** | Additional data to pass to server | [optional] 
**RequiredFile** | Pointer to [***os.File**](*os.File.md) | file to upload | 

## Methods

### NewInlineObject5

`func NewInlineObject5(requiredFile *os.File, ) *InlineObject5`

NewInlineObject5 instantiates a new InlineObject5 object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewInlineObject5WithDefaults

`func NewInlineObject5WithDefaults() *InlineObject5`

NewInlineObject5WithDefaults instantiates a new InlineObject5 object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetAdditionalMetadata

`func (o *InlineObject5) GetAdditionalMetadata() string`

GetAdditionalMetadata returns the AdditionalMetadata field if non-nil, zero value otherwise.

### GetAdditionalMetadataOk

`func (o *InlineObject5) GetAdditionalMetadataOk() (string, bool)`

GetAdditionalMetadataOk returns a tuple with the AdditionalMetadata field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasAdditionalMetadata

`func (o *InlineObject5) HasAdditionalMetadata() bool`

HasAdditionalMetadata returns a boolean if a field has been set.

### SetAdditionalMetadata

`func (o *InlineObject5) SetAdditionalMetadata(v string)`

SetAdditionalMetadata gets a reference to the given string and assigns it to the AdditionalMetadata field.

### GetRequiredFile

`func (o *InlineObject5) GetRequiredFile() *os.File`

GetRequiredFile returns the RequiredFile field if non-nil, zero value otherwise.

### GetRequiredFileOk

`func (o *InlineObject5) GetRequiredFileOk() (*os.File, bool)`

GetRequiredFileOk returns a tuple with the RequiredFile field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasRequiredFile

`func (o *InlineObject5) HasRequiredFile() bool`

HasRequiredFile returns a boolean if a field has been set.

### SetRequiredFile

`func (o *InlineObject5) SetRequiredFile(v *os.File)`

SetRequiredFile gets a reference to the given *os.File and assigns it to the RequiredFile field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


