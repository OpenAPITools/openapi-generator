# Dog

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Breed** | Pointer to **string** |  | [optional] 

## Methods

### NewDog

`func NewDog() *Dog`

NewDog instantiates a new Dog object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewDogWithDefaults

`func NewDogWithDefaults() *Dog`

NewDogWithDefaults instantiates a new Dog object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetBreed

`func (o *Dog) GetBreed() string`

GetBreed returns the Breed field if non-nil, zero value otherwise.

### GetBreedOk

`func (o *Dog) GetBreedOk() (*string, bool)`

GetBreedOk returns a tuple with the Breed field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBreed

`func (o *Dog) SetBreed(v string)`

SetBreed sets Breed field to given value.

### HasBreed

`func (o *Dog) HasBreed() bool`

HasBreed returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


