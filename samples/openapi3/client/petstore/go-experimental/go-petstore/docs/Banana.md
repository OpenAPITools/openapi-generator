# Banana

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**LengthCm** | Pointer to **float32** |  | [optional] 
**Color** | Pointer to **string** |  | [optional] 

## Methods

### NewBanana

`func NewBanana() *Banana`

NewBanana instantiates a new Banana object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewBananaWithDefaults

`func NewBananaWithDefaults() *Banana`

NewBananaWithDefaults instantiates a new Banana object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetLengthCm

`func (o *Banana) GetLengthCm() float32`

GetLengthCm returns the LengthCm field if non-nil, zero value otherwise.

### GetLengthCmOk

`func (o *Banana) GetLengthCmOk() (float32, bool)`

GetLengthCmOk returns a tuple with the LengthCm field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasLengthCm

`func (o *Banana) HasLengthCm() bool`

HasLengthCm returns a boolean if a field has been set.

### SetLengthCm

`func (o *Banana) SetLengthCm(v float32)`

SetLengthCm gets a reference to the given float32 and assigns it to the LengthCm field.

### GetColor

`func (o *Banana) GetColor() string`

GetColor returns the Color field if non-nil, zero value otherwise.

### GetColorOk

`func (o *Banana) GetColorOk() (string, bool)`

GetColorOk returns a tuple with the Color field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasColor

`func (o *Banana) HasColor() bool`

HasColor returns a boolean if a field has been set.

### SetColor

`func (o *Banana) SetColor(v string)`

SetColor gets a reference to the given string and assigns it to the Color field.


### AsFruit

`func (s *Banana) AsFruit() Fruit`

Convenience method to wrap this instance of Banana in Fruit

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


