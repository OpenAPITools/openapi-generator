# BananaReq

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**LengthCm** | Pointer to **float32** |  | 
**Sweet** | Pointer to **bool** |  | [optional] 

## Methods

### NewBananaReq

`func NewBananaReq(lengthCm float32, ) *BananaReq`

NewBananaReq instantiates a new BananaReq object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewBananaReqWithDefaults

`func NewBananaReqWithDefaults() *BananaReq`

NewBananaReqWithDefaults instantiates a new BananaReq object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetLengthCm

`func (o *BananaReq) GetLengthCm() float32`

GetLengthCm returns the LengthCm field if non-nil, zero value otherwise.

### GetLengthCmOk

`func (o *BananaReq) GetLengthCmOk() (float32, bool)`

GetLengthCmOk returns a tuple with the LengthCm field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasLengthCm

`func (o *BananaReq) HasLengthCm() bool`

HasLengthCm returns a boolean if a field has been set.

### SetLengthCm

`func (o *BananaReq) SetLengthCm(v float32)`

SetLengthCm gets a reference to the given float32 and assigns it to the LengthCm field.

### GetSweet

`func (o *BananaReq) GetSweet() bool`

GetSweet returns the Sweet field if non-nil, zero value otherwise.

### GetSweetOk

`func (o *BananaReq) GetSweetOk() (bool, bool)`

GetSweetOk returns a tuple with the Sweet field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasSweet

`func (o *BananaReq) HasSweet() bool`

HasSweet returns a boolean if a field has been set.

### SetSweet

`func (o *BananaReq) SetSweet(v bool)`

SetSweet gets a reference to the given bool and assigns it to the Sweet field.


### AsFruitReq

`func (s *BananaReq) AsFruitReq() FruitReq`

Convenience method to wrap this instance of BananaReq in FruitReq

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


