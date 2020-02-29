# Zebra

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Type** | Pointer to **string** |  | [optional] 
**ClassName** | Pointer to **string** |  | 

## Methods

### NewZebra

`func NewZebra(className string, ) *Zebra`

NewZebra instantiates a new Zebra object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewZebraWithDefaults

`func NewZebraWithDefaults() *Zebra`

NewZebraWithDefaults instantiates a new Zebra object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetType

`func (o *Zebra) GetType() string`

GetType returns the Type field if non-nil, zero value otherwise.

### GetTypeOk

`func (o *Zebra) GetTypeOk() (string, bool)`

GetTypeOk returns a tuple with the Type field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasType

`func (o *Zebra) HasType() bool`

HasType returns a boolean if a field has been set.

### SetType

`func (o *Zebra) SetType(v string)`

SetType gets a reference to the given string and assigns it to the Type field.

### GetClassName

`func (o *Zebra) GetClassName() string`

GetClassName returns the ClassName field if non-nil, zero value otherwise.

### GetClassNameOk

`func (o *Zebra) GetClassNameOk() (string, bool)`

GetClassNameOk returns a tuple with the ClassName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasClassName

`func (o *Zebra) HasClassName() bool`

HasClassName returns a boolean if a field has been set.

### SetClassName

`func (o *Zebra) SetClassName(v string)`

SetClassName gets a reference to the given string and assigns it to the ClassName field.


### AsMammal

`func (s *Zebra) AsMammal() Mammal`

Convenience method to wrap this instance of Zebra in Mammal

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


