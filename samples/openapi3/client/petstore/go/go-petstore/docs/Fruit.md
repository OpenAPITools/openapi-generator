# Fruit

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Color** | Pointer to **string** |  | [optional] 
**Cultivar** | Pointer to **string** |  | [optional] 
**LengthCm** | Pointer to **float32** |  | [optional] 

## Methods

### NewFruit

`func NewFruit() *Fruit`

NewFruit instantiates a new Fruit object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewFruitWithDefaults

`func NewFruitWithDefaults() *Fruit`

NewFruitWithDefaults instantiates a new Fruit object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetColor

`func (o *Fruit) GetColor() string`

GetColor returns the Color field if non-nil, zero value otherwise.

### GetColorOk

`func (o *Fruit) GetColorOk() (*string, bool)`

GetColorOk returns a tuple with the Color field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetColor

`func (o *Fruit) SetColor(v string)`

SetColor sets Color field to given value.

### HasColor

`func (o *Fruit) HasColor() bool`

HasColor returns a boolean if a field has been set.

### GetCultivar

`func (o *Fruit) GetCultivar() string`

GetCultivar returns the Cultivar field if non-nil, zero value otherwise.

### GetCultivarOk

`func (o *Fruit) GetCultivarOk() (*string, bool)`

GetCultivarOk returns a tuple with the Cultivar field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCultivar

`func (o *Fruit) SetCultivar(v string)`

SetCultivar sets Cultivar field to given value.

### HasCultivar

`func (o *Fruit) HasCultivar() bool`

HasCultivar returns a boolean if a field has been set.

### GetLengthCm

`func (o *Fruit) GetLengthCm() float32`

GetLengthCm returns the LengthCm field if non-nil, zero value otherwise.

### GetLengthCmOk

`func (o *Fruit) GetLengthCmOk() (*float32, bool)`

GetLengthCmOk returns a tuple with the LengthCm field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLengthCm

`func (o *Fruit) SetLengthCm(v float32)`

SetLengthCm sets LengthCm field to given value.

### HasLengthCm

`func (o *Fruit) HasLengthCm() bool`

HasLengthCm returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


