# Pet

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | Pointer to **int64** |  | [optional] 
**Category** | Pointer to [**Category**](Category.md) |  | [optional] 
**Name** | **string** |  | 
**PhotoUrls** | **[]string** |  | 
**Tags** | Pointer to [**[]Tag**](Tag.md) |  | [optional] 
**Status** | Pointer to **string** | pet status in the store | [optional] 

## Methods

### NewPet

`func NewPet(name string, photoUrls []string, ) *Pet`

NewPet instantiates a new Pet object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewPetWithDefaults

`func NewPetWithDefaults() *Pet`

NewPetWithDefaults instantiates a new Pet object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *Pet) GetId() int64`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *Pet) GetIdOk() (*int64, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *Pet) SetId(v int64)`

SetId sets Id field to given value.

### HasId

`func (o *Pet) HasId() bool`

HasId returns a boolean if a field has been set.

### GetCategory

`func (o *Pet) GetCategory() Category`

GetCategory returns the Category field if non-nil, zero value otherwise.

### GetCategoryOk

`func (o *Pet) GetCategoryOk() (*Category, bool)`

GetCategoryOk returns a tuple with the Category field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCategory

`func (o *Pet) SetCategory(v Category)`

SetCategory sets Category field to given value.

### HasCategory

`func (o *Pet) HasCategory() bool`

HasCategory returns a boolean if a field has been set.

### GetName

`func (o *Pet) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *Pet) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *Pet) SetName(v string)`

SetName sets Name field to given value.


### GetPhotoUrls

`func (o *Pet) GetPhotoUrls() []string`

GetPhotoUrls returns the PhotoUrls field if non-nil, zero value otherwise.

### GetPhotoUrlsOk

`func (o *Pet) GetPhotoUrlsOk() (*[]string, bool)`

GetPhotoUrlsOk returns a tuple with the PhotoUrls field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPhotoUrls

`func (o *Pet) SetPhotoUrls(v []string)`

SetPhotoUrls sets PhotoUrls field to given value.


### GetTags

`func (o *Pet) GetTags() []Tag`

GetTags returns the Tags field if non-nil, zero value otherwise.

### GetTagsOk

`func (o *Pet) GetTagsOk() (*[]Tag, bool)`

GetTagsOk returns a tuple with the Tags field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTags

`func (o *Pet) SetTags(v []Tag)`

SetTags sets Tags field to given value.

### HasTags

`func (o *Pet) HasTags() bool`

HasTags returns a boolean if a field has been set.

### GetStatus

`func (o *Pet) GetStatus() string`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *Pet) GetStatusOk() (*string, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStatus

`func (o *Pet) SetStatus(v string)`

SetStatus sets Status field to given value.

### HasStatus

`func (o *Pet) HasStatus() bool`

HasStatus returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


