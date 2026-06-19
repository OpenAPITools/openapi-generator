# Bird

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Size** | Pointer to **string** |  | [optional] 
**Color** | Pointer to **string** |  | [optional] 

## Methods

### NewBird

`func NewBird() *Bird`

NewBird instantiates a new Bird object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewBirdWithDefaults

`func NewBirdWithDefaults() *Bird`

NewBirdWithDefaults instantiates a new Bird object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetSize

`func (o *Bird) GetSize() string`

GetSize returns the Size field if non-nil, zero value otherwise.

### GetSizeOk

`func (o *Bird) GetSizeOk() (*string, bool)`

GetSizeOk returns a tuple with the Size field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSize

`func (o *Bird) SetSize(v string)`

SetSize sets Size field to given value.

### HasSize

`func (o *Bird) HasSize() bool`

HasSize returns a boolean if a field has been set.

### GetColor

`func (o *Bird) GetColor() string`

GetColor returns the Color field if non-nil, zero value otherwise.

### GetColorOk

`func (o *Bird) GetColorOk() (*string, bool)`

GetColorOk returns a tuple with the Color field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetColor

`func (o *Bird) SetColor(v string)`

SetColor sets Color field to given value.

### HasColor

`func (o *Bird) HasColor() bool`

HasColor returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


