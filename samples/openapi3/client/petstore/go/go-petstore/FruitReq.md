# FruitReq

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Cultivar** | **string** |  | 
**Mealy** | Pointer to **bool** |  | [optional] 
**LengthCm** | **float32** |  | 
**Sweet** | Pointer to **bool** |  | [optional] 

## Methods

### NewFruitReq

`func NewFruitReq(cultivar string, lengthCm float32, ) *FruitReq`

NewFruitReq instantiates a new FruitReq object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewFruitReqWithDefaults

`func NewFruitReqWithDefaults() *FruitReq`

NewFruitReqWithDefaults instantiates a new FruitReq object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetCultivar

`func (o *FruitReq) GetCultivar() string`

GetCultivar returns the Cultivar field if non-nil, zero value otherwise.

### GetCultivarOk

`func (o *FruitReq) GetCultivarOk() (*string, bool)`

GetCultivarOk returns a tuple with the Cultivar field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCultivar

`func (o *FruitReq) SetCultivar(v string)`

SetCultivar sets Cultivar field to given value.


### GetMealy

`func (o *FruitReq) GetMealy() bool`

GetMealy returns the Mealy field if non-nil, zero value otherwise.

### GetMealyOk

`func (o *FruitReq) GetMealyOk() (*bool, bool)`

GetMealyOk returns a tuple with the Mealy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMealy

`func (o *FruitReq) SetMealy(v bool)`

SetMealy sets Mealy field to given value.

### HasMealy

`func (o *FruitReq) HasMealy() bool`

HasMealy returns a boolean if a field has been set.

### GetLengthCm

`func (o *FruitReq) GetLengthCm() float32`

GetLengthCm returns the LengthCm field if non-nil, zero value otherwise.

### GetLengthCmOk

`func (o *FruitReq) GetLengthCmOk() (*float32, bool)`

GetLengthCmOk returns a tuple with the LengthCm field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLengthCm

`func (o *FruitReq) SetLengthCm(v float32)`

SetLengthCm sets LengthCm field to given value.


### GetSweet

`func (o *FruitReq) GetSweet() bool`

GetSweet returns the Sweet field if non-nil, zero value otherwise.

### GetSweetOk

`func (o *FruitReq) GetSweetOk() (*bool, bool)`

GetSweetOk returns a tuple with the Sweet field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSweet

`func (o *FruitReq) SetSweet(v bool)`

SetSweet sets Sweet field to given value.

### HasSweet

`func (o *FruitReq) HasSweet() bool`

HasSweet returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


