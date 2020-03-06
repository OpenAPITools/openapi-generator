# Whale

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**HasBaleen** | Pointer to **bool** |  | [optional] 
**HasTeeth** | Pointer to **bool** |  | [optional] 
**ClassName** | Pointer to **string** |  | 

## Methods

### NewWhale

`func NewWhale(className string, ) *Whale`

NewWhale instantiates a new Whale object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewWhaleWithDefaults

`func NewWhaleWithDefaults() *Whale`

NewWhaleWithDefaults instantiates a new Whale object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetHasBaleen

`func (o *Whale) GetHasBaleen() bool`

GetHasBaleen returns the HasBaleen field if non-nil, zero value otherwise.

### GetHasBaleenOk

`func (o *Whale) GetHasBaleenOk() (bool, bool)`

GetHasBaleenOk returns a tuple with the HasBaleen field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasHasBaleen

`func (o *Whale) HasHasBaleen() bool`

HasHasBaleen returns a boolean if a field has been set.

### SetHasBaleen

`func (o *Whale) SetHasBaleen(v bool)`

SetHasBaleen gets a reference to the given bool and assigns it to the HasBaleen field.

### GetHasTeeth

`func (o *Whale) GetHasTeeth() bool`

GetHasTeeth returns the HasTeeth field if non-nil, zero value otherwise.

### GetHasTeethOk

`func (o *Whale) GetHasTeethOk() (bool, bool)`

GetHasTeethOk returns a tuple with the HasTeeth field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasHasTeeth

`func (o *Whale) HasHasTeeth() bool`

HasHasTeeth returns a boolean if a field has been set.

### SetHasTeeth

`func (o *Whale) SetHasTeeth(v bool)`

SetHasTeeth gets a reference to the given bool and assigns it to the HasTeeth field.

### GetClassName

`func (o *Whale) GetClassName() string`

GetClassName returns the ClassName field if non-nil, zero value otherwise.

### GetClassNameOk

`func (o *Whale) GetClassNameOk() (string, bool)`

GetClassNameOk returns a tuple with the ClassName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasClassName

`func (o *Whale) HasClassName() bool`

HasClassName returns a boolean if a field has been set.

### SetClassName

`func (o *Whale) SetClassName(v string)`

SetClassName gets a reference to the given string and assigns it to the ClassName field.


### AsMammal

`func (s *Whale) AsMammal() Mammal`

Convenience method to wrap this instance of Whale in Mammal

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


