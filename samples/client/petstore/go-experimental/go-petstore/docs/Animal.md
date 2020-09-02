# Animal

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ClassName** | **string** |  | 
**Color** | Pointer to **string** |  | [optional] [default to "red"]

## Methods

### NewAnimal

`func NewAnimal(className string, ) *Animal`

NewAnimal instantiates a new Animal object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewAnimalWithDefaults

`func NewAnimalWithDefaults() *Animal`

NewAnimalWithDefaults instantiates a new Animal object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetClassName

`func (o *Animal) GetClassName() string`

GetClassName returns the ClassName field if non-nil, zero value otherwise.

### GetClassNameOk

`func (o *Animal) GetClassNameOk() (*string, bool)`

GetClassNameOk returns a tuple with the ClassName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetClassName

`func (o *Animal) SetClassName(v string)`

SetClassName sets ClassName field to given value.


### GetColor

`func (o *Animal) GetColor() string`

GetColor returns the Color field if non-nil, zero value otherwise.

### GetColorOk

`func (o *Animal) GetColorOk() (*string, bool)`

GetColorOk returns a tuple with the Color field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetColor

`func (o *Animal) SetColor(v string)`

SetColor sets Color field to given value.

### HasColor

`func (o *Animal) HasColor() bool`

HasColor returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


