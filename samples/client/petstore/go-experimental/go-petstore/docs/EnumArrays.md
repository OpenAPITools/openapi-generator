# EnumArrays

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**JustSymbol** | Pointer to [**EnumArraysJustSymbol**](EnumArraysJustSymbol.md) |  | [optional] 
**ArrayEnum** | Pointer to [**[]EnumArraysArrayEnumItems**](EnumArraysArrayEnumItems.md) |  | [optional] 

## Methods

### NewEnumArrays

`func NewEnumArrays() *EnumArrays`

NewEnumArrays instantiates a new EnumArrays object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewEnumArraysWithDefaults

`func NewEnumArraysWithDefaults() *EnumArrays`

NewEnumArraysWithDefaults instantiates a new EnumArrays object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetJustSymbol

`func (o *EnumArrays) GetJustSymbol() EnumArraysJustSymbol`

GetJustSymbol returns the JustSymbol field if non-nil, zero value otherwise.

### GetJustSymbolOk

<<<<<<< HEAD
`func (o *EnumArrays) GetJustSymbolOk() (EnumArraysJustSymbol, bool)`
=======
`func (o *EnumArrays) GetJustSymbolOk() (*string, bool)`
>>>>>>> origin/master

GetJustSymbolOk returns a tuple with the JustSymbol field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetJustSymbol

`func (o *EnumArrays) SetJustSymbol(v string)`

SetJustSymbol sets JustSymbol field to given value.

### HasJustSymbol

<<<<<<< HEAD
`func (o *EnumArrays) SetJustSymbol(v EnumArraysJustSymbol)`

SetJustSymbol gets a reference to the given EnumArraysJustSymbol and assigns it to the JustSymbol field.
=======
`func (o *EnumArrays) HasJustSymbol() bool`

HasJustSymbol returns a boolean if a field has been set.
>>>>>>> origin/master

### GetArrayEnum

`func (o *EnumArrays) GetArrayEnum() []EnumArraysArrayEnumItems`

GetArrayEnum returns the ArrayEnum field if non-nil, zero value otherwise.

### GetArrayEnumOk

<<<<<<< HEAD
`func (o *EnumArrays) GetArrayEnumOk() ([]EnumArraysArrayEnumItems, bool)`
=======
`func (o *EnumArrays) GetArrayEnumOk() (*[]string, bool)`
>>>>>>> origin/master

GetArrayEnumOk returns a tuple with the ArrayEnum field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayEnum

`func (o *EnumArrays) SetArrayEnum(v []string)`

SetArrayEnum sets ArrayEnum field to given value.

### HasArrayEnum

<<<<<<< HEAD
`func (o *EnumArrays) SetArrayEnum(v []EnumArraysArrayEnumItems)`

SetArrayEnum gets a reference to the given []EnumArraysArrayEnumItems and assigns it to the ArrayEnum field.
=======
`func (o *EnumArrays) HasArrayEnum() bool`

HasArrayEnum returns a boolean if a field has been set.
>>>>>>> origin/master


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


