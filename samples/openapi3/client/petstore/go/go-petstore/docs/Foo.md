# Foo

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Bar** | Pointer to **string** |  | [optional] [default to "bar"]

## Methods

### NewFoo

`func NewFoo() *Foo`

NewFoo instantiates a new Foo object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewFooWithDefaults

`func NewFooWithDefaults() *Foo`

NewFooWithDefaults instantiates a new Foo object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetBar

`func (o *Foo) GetBar() string`

GetBar returns the Bar field if non-nil, zero value otherwise.

### GetBarOk

`func (o *Foo) GetBarOk() (*string, bool)`

GetBarOk returns a tuple with the Bar field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBar

`func (o *Foo) SetBar(v string)`

SetBar sets Bar field to given value.

### HasBar

`func (o *Foo) HasBar() bool`

HasBar returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


