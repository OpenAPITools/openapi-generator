# Holder

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Child** | [**Nested**](Nested.md) |  | 

## Methods

### NewHolder

`func NewHolder(child Nested, ) *Holder`

NewHolder instantiates a new Holder object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewHolderWithDefaults

`func NewHolderWithDefaults() *Holder`

NewHolderWithDefaults instantiates a new Holder object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetChild

`func (o *Holder) GetChild() Nested`

GetChild returns the Child field if non-nil, zero value otherwise.

### GetChildOk

`func (o *Holder) GetChildOk() (*Nested, bool)`

GetChildOk returns a tuple with the Child field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChild

`func (o *Holder) SetChild(v Nested)`

SetChild sets Child field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


