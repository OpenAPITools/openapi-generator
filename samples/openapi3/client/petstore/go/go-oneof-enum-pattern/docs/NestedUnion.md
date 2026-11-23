# NestedUnion

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Child** | [**Nested**](Nested.md) |  | 
**Kind** | **string** |  | 

## Methods

### NewNestedUnion

`func NewNestedUnion(child Nested, kind string, ) *NestedUnion`

NewNestedUnion instantiates a new NestedUnion object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewNestedUnionWithDefaults

`func NewNestedUnionWithDefaults() *NestedUnion`

NewNestedUnionWithDefaults instantiates a new NestedUnion object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetChild

`func (o *NestedUnion) GetChild() Nested`

GetChild returns the Child field if non-nil, zero value otherwise.

### GetChildOk

`func (o *NestedUnion) GetChildOk() (*Nested, bool)`

GetChildOk returns a tuple with the Child field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChild

`func (o *NestedUnion) SetChild(v Nested)`

SetChild sets Child field to given value.


### GetKind

`func (o *NestedUnion) GetKind() string`

GetKind returns the Kind field if non-nil, zero value otherwise.

### GetKindOk

`func (o *NestedUnion) GetKindOk() (*string, bool)`

GetKindOk returns a tuple with the Kind field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetKind

`func (o *NestedUnion) SetKind(v string)`

SetKind sets Kind field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


