# Pet

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ID** | Pointer to **int64** |  | [optional] 
**Category** | Pointer to [**Category**](Category.md) |  | [optional] 
**Name** | Pointer to **string** |  | 
**PhotoURLs** | Pointer to **[]string** |  | 
**Tags** | Pointer to [**[]Tag**](Tag.md) |  | [optional] 
**Status** | Pointer to **string** | pet status in the store | [optional] 

## Methods

### GetID

`func (o *Pet) GetID() int64`

GetID returns the ID field if non-nil, zero value otherwise.

### GetIDOk

`func (o *Pet) GetIDOk() (int64, bool)`

GetIDOk returns a tuple with the ID field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasID

`func (o *Pet) HasID() bool`

HasID returns a boolean if a field has been set.

### SetID

`func (o *Pet) SetID(v int64)`

SetID gets a reference to the given int64 and assigns it to the ID field.

### GetCategory

`func (o *Pet) GetCategory() Category`

GetCategory returns the Category field if non-nil, zero value otherwise.

### GetCategoryOk

`func (o *Pet) GetCategoryOk() (Category, bool)`

GetCategoryOk returns a tuple with the Category field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasCategory

`func (o *Pet) HasCategory() bool`

HasCategory returns a boolean if a field has been set.

### SetCategory

`func (o *Pet) SetCategory(v Category)`

SetCategory gets a reference to the given Category and assigns it to the Category field.

### GetName

`func (o *Pet) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *Pet) GetNameOk() (string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasName

`func (o *Pet) HasName() bool`

HasName returns a boolean if a field has been set.

### SetName

`func (o *Pet) SetName(v string)`

SetName gets a reference to the given string and assigns it to the Name field.

### GetPhotoURLs

`func (o *Pet) GetPhotoURLs() []string`

GetPhotoURLs returns the PhotoURLs field if non-nil, zero value otherwise.

### GetPhotoURLsOk

`func (o *Pet) GetPhotoURLsOk() ([]string, bool)`

GetPhotoURLsOk returns a tuple with the PhotoURLs field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPhotoURLs

`func (o *Pet) HasPhotoURLs() bool`

HasPhotoURLs returns a boolean if a field has been set.

### SetPhotoURLs

`func (o *Pet) SetPhotoURLs(v []string)`

SetPhotoURLs gets a reference to the given []string and assigns it to the PhotoURLs field.

### GetTags

`func (o *Pet) GetTags() []Tag`

GetTags returns the Tags field if non-nil, zero value otherwise.

### GetTagsOk

`func (o *Pet) GetTagsOk() ([]Tag, bool)`

GetTagsOk returns a tuple with the Tags field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasTags

`func (o *Pet) HasTags() bool`

HasTags returns a boolean if a field has been set.

### SetTags

`func (o *Pet) SetTags(v []Tag)`

SetTags gets a reference to the given []Tag and assigns it to the Tags field.

### GetStatus

`func (o *Pet) GetStatus() string`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *Pet) GetStatusOk() (string, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasStatus

`func (o *Pet) HasStatus() bool`

HasStatus returns a boolean if a field has been set.

### SetStatus

`func (o *Pet) SetStatus(v string)`

SetStatus gets a reference to the given string and assigns it to the Status field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


