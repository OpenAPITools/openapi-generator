# AppleReq

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Cultivar** | **string** |  | 
**Mealy** | Pointer to **bool** |  | [optional] 

## Methods

### NewAppleReq

`func NewAppleReq(cultivar string, ) *AppleReq`

NewAppleReq instantiates a new AppleReq object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewAppleReqWithDefaults

`func NewAppleReqWithDefaults() *AppleReq`

NewAppleReqWithDefaults instantiates a new AppleReq object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetCultivar

`func (o *AppleReq) GetCultivar() string`

GetCultivar returns the Cultivar field if non-nil, zero value otherwise.

### GetCultivarOk

`func (o *AppleReq) GetCultivarOk() (*string, bool)`

GetCultivarOk returns a tuple with the Cultivar field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCultivar

`func (o *AppleReq) SetCultivar(v string)`

SetCultivar sets Cultivar field to given value.


### GetMealy

`func (o *AppleReq) GetMealy() bool`

GetMealy returns the Mealy field if non-nil, zero value otherwise.

### GetMealyOk

`func (o *AppleReq) GetMealyOk() (*bool, bool)`

GetMealyOk returns a tuple with the Mealy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMealy

`func (o *AppleReq) SetMealy(v bool)`

SetMealy sets Mealy field to given value.

### HasMealy

`func (o *AppleReq) HasMealy() bool`

HasMealy returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


