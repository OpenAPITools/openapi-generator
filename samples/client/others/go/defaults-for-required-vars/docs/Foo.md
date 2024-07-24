# Foo

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Bar** | **string** |  | [default to "value1"]
**Baz** | **string** |  | [default to "value2"]
**Qux** | **string** |  | 
**Thud** | Pointer to **int32** |  | [optional] [default to 42]

## Methods

### NewFoo

`func NewFoo(bar string, baz string, qux string, ) *Foo`

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


### GetBaz

`func (o *Foo) GetBaz() string`

GetBaz returns the Baz field if non-nil, zero value otherwise.

### GetBazOk

`func (o *Foo) GetBazOk() (*string, bool)`

GetBazOk returns a tuple with the Baz field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBaz

`func (o *Foo) SetBaz(v string)`

SetBaz sets Baz field to given value.


### GetQux

`func (o *Foo) GetQux() string`

GetQux returns the Qux field if non-nil, zero value otherwise.

### GetQuxOk

`func (o *Foo) GetQuxOk() (*string, bool)`

GetQuxOk returns a tuple with the Qux field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetQux

`func (o *Foo) SetQux(v string)`

SetQux sets Qux field to given value.


### GetThud

`func (o *Foo) GetThud() int32`

GetThud returns the Thud field if non-nil, zero value otherwise.

### GetThudOk

`func (o *Foo) GetThudOk() (*int32, bool)`

GetThudOk returns a tuple with the Thud field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetThud

`func (o *Foo) SetThud(v int32)`

SetThud sets Thud field to given value.

### HasThud

`func (o *Foo) HasThud() bool`

HasThud returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


