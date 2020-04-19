# EnumTest

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**EnumString** | Pointer to [**EnumTestEnumString**](Enum_TestEnumString.md) |  | [optional] 
**EnumStringRequired** | Pointer to [**EnumTestEnumString**](Enum_TestEnumString.md) |  | 
**EnumInteger** | Pointer to [**EnumTestEnumInteger**](Enum_TestEnumInteger.md) |  | [optional] 
**EnumNumber** | Pointer to [**EnumQueryDouble**](enum_query_double.md) |  | [optional] 
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

`func (o *EnumTest) GetEnumString() EnumTestEnumString`

GetEnumString returns the EnumString field if non-nil, zero value otherwise.

### GetEnumStringOk

<<<<<<< HEAD
`func (o *EnumTest) GetEnumStringOk() (EnumTestEnumString, bool)`
=======
`func (o *EnumTest) GetEnumStringOk() (*string, bool)`
>>>>>>> origin/master

GetEnumStringOk returns a tuple with the EnumString field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumString

`func (o *EnumTest) SetEnumString(v string)`

SetEnumString sets EnumString field to given value.

### HasEnumString

<<<<<<< HEAD
`func (o *EnumTest) SetEnumString(v EnumTestEnumString)`

SetEnumString gets a reference to the given EnumTestEnumString and assigns it to the EnumString field.
=======
`func (o *EnumTest) HasEnumString() bool`

HasEnumString returns a boolean if a field has been set.
>>>>>>> origin/master

### GetEnumStringRequired

`func (o *EnumTest) GetEnumStringRequired() EnumTestEnumString`

GetEnumStringRequired returns the EnumStringRequired field if non-nil, zero value otherwise.

### GetEnumStringRequiredOk

<<<<<<< HEAD
`func (o *EnumTest) GetEnumStringRequiredOk() (EnumTestEnumString, bool)`
=======
`func (o *EnumTest) GetEnumStringRequiredOk() (*string, bool)`
>>>>>>> origin/master

GetEnumStringRequiredOk returns a tuple with the EnumStringRequired field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumStringRequired

`func (o *EnumTest) SetEnumStringRequired(v EnumTestEnumString)`

<<<<<<< HEAD
SetEnumStringRequired gets a reference to the given EnumTestEnumString and assigns it to the EnumStringRequired field.
=======
SetEnumStringRequired sets EnumStringRequired field to given value.

>>>>>>> origin/master

### GetEnumInteger

`func (o *EnumTest) GetEnumInteger() EnumTestEnumInteger`

GetEnumInteger returns the EnumInteger field if non-nil, zero value otherwise.

### GetEnumIntegerOk

<<<<<<< HEAD
`func (o *EnumTest) GetEnumIntegerOk() (EnumTestEnumInteger, bool)`
=======
`func (o *EnumTest) GetEnumIntegerOk() (*int32, bool)`
>>>>>>> origin/master

GetEnumIntegerOk returns a tuple with the EnumInteger field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumInteger

`func (o *EnumTest) SetEnumInteger(v int32)`

SetEnumInteger sets EnumInteger field to given value.

### HasEnumInteger

<<<<<<< HEAD
`func (o *EnumTest) SetEnumInteger(v EnumTestEnumInteger)`

SetEnumInteger gets a reference to the given EnumTestEnumInteger and assigns it to the EnumInteger field.
=======
`func (o *EnumTest) HasEnumInteger() bool`

HasEnumInteger returns a boolean if a field has been set.
>>>>>>> origin/master

### GetEnumNumber

`func (o *EnumTest) GetEnumNumber() EnumQueryDouble`

GetEnumNumber returns the EnumNumber field if non-nil, zero value otherwise.

### GetEnumNumberOk

<<<<<<< HEAD
`func (o *EnumTest) GetEnumNumberOk() (EnumQueryDouble, bool)`
=======
`func (o *EnumTest) GetEnumNumberOk() (*float64, bool)`
>>>>>>> origin/master

GetEnumNumberOk returns a tuple with the EnumNumber field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEnumNumber

`func (o *EnumTest) SetEnumNumber(v float64)`

SetEnumNumber sets EnumNumber field to given value.

### HasEnumNumber

<<<<<<< HEAD
`func (o *EnumTest) SetEnumNumber(v EnumQueryDouble)`

SetEnumNumber gets a reference to the given EnumQueryDouble and assigns it to the EnumNumber field.
=======
`func (o *EnumTest) HasEnumNumber() bool`

HasEnumNumber returns a boolean if a field has been set.
>>>>>>> origin/master

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


