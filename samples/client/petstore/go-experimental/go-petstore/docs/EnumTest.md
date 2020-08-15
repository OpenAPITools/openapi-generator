# EnumTest

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**EnumString** | Pointer to **string** |  | [optional] 
**EnumStringRequired** | **string** |  | 
**EnumInteger** | Pointer to **int32** |  | [optional] 
**EnumNumber** | Pointer to **float64** |  | [optional] 
**OuterEnum** | Pointer to [**OuterEnum**](OuterEnum.md) |  | [optional] 

## Methods

### NewEnumTest

`func NewEnumTest(enumStringRequired string, ) *EnumTest`

NewEnumTest instantiates a new EnumTest object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewEnumTestWithDefaults

`func NewEnumTestWithDefaults() *EnumTest`

NewEnumTestWithDefaults instantiates a new EnumTest object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetEnumString

`func (o *EnumTest) GetEnumString() string`

GetEnumString returns the EnumString field if non-nil, zero value otherwise.

### GetEnumStringOk

`func (o *EnumTest) GetEnumStringOk() (*string, bool)`

GetEnumStringOk returns a tuple with the EnumString field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumString

`func (o *EnumTest) SetEnumString(v string)`

SetEnumString sets EnumString field to given value.

### HasEnumString

`func (o *EnumTest) HasEnumString() bool`

HasEnumString returns a boolean if a field has been set.

### GetEnumStringRequired

`func (o *EnumTest) GetEnumStringRequired() string`

GetEnumStringRequired returns the EnumStringRequired field if non-nil, zero value otherwise.

### GetEnumStringRequiredOk

`func (o *EnumTest) GetEnumStringRequiredOk() (*string, bool)`

GetEnumStringRequiredOk returns a tuple with the EnumStringRequired field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumStringRequired

`func (o *EnumTest) SetEnumStringRequired(v string)`

SetEnumStringRequired sets EnumStringRequired field to given value.


### GetEnumInteger

`func (o *EnumTest) GetEnumInteger() int32`

GetEnumInteger returns the EnumInteger field if non-nil, zero value otherwise.

### GetEnumIntegerOk

`func (o *EnumTest) GetEnumIntegerOk() (*int32, bool)`

GetEnumIntegerOk returns a tuple with the EnumInteger field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumInteger

`func (o *EnumTest) SetEnumInteger(v int32)`

SetEnumInteger sets EnumInteger field to given value.

### HasEnumInteger

`func (o *EnumTest) HasEnumInteger() bool`

HasEnumInteger returns a boolean if a field has been set.

### GetEnumNumber

`func (o *EnumTest) GetEnumNumber() float64`

GetEnumNumber returns the EnumNumber field if non-nil, zero value otherwise.

### GetEnumNumberOk

`func (o *EnumTest) GetEnumNumberOk() (*float64, bool)`

GetEnumNumberOk returns a tuple with the EnumNumber field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumNumber

`func (o *EnumTest) SetEnumNumber(v float64)`

SetEnumNumber sets EnumNumber field to given value.

### HasEnumNumber

`func (o *EnumTest) HasEnumNumber() bool`

HasEnumNumber returns a boolean if a field has been set.

### GetOuterEnum

`func (o *EnumTest) GetOuterEnum() OuterEnum`

GetOuterEnum returns the OuterEnum field if non-nil, zero value otherwise.

### GetOuterEnumOk

`func (o *EnumTest) GetOuterEnumOk() (*OuterEnum, bool)`

GetOuterEnumOk returns a tuple with the OuterEnum field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOuterEnum

`func (o *EnumTest) SetOuterEnum(v OuterEnum)`

SetOuterEnum sets OuterEnum field to given value.

### HasOuterEnum

`func (o *EnumTest) HasOuterEnum() bool`

HasOuterEnum returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


