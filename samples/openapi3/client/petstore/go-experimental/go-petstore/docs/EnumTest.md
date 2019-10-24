# EnumTest

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**EnumString** | Pointer to **string** |  | [optional] 
**EnumStringRequired** | Pointer to **string** |  | 
**EnumInteger** | Pointer to **int32** |  | [optional] 
**EnumNumber** | Pointer to **float64** |  | [optional] 
**OuterEnum** | Pointer to [**NullableOuterEnum**](OuterEnum.md) |  | [optional] 
**OuterEnumInteger** | Pointer to [**OuterEnumInteger**](OuterEnumInteger.md) |  | [optional] 
**OuterEnumDefaultValue** | Pointer to [**OuterEnumDefaultValue**](OuterEnumDefaultValue.md) |  | [optional] 
**OuterEnumIntegerDefaultValue** | Pointer to [**OuterEnumIntegerDefaultValue**](OuterEnumIntegerDefaultValue.md) |  | [optional] 

## Methods

### GetEnumString

`func (o *EnumTest) GetEnumString() string`

GetEnumString returns the EnumString field if non-nil, zero value otherwise.

### GetEnumStringOk

`func (o *EnumTest) GetEnumStringOk() (string, bool)`

GetEnumStringOk returns a tuple with the EnumString field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasEnumString

`func (o *EnumTest) HasEnumString() bool`

HasEnumString returns a boolean if a field has been set.

### SetEnumString

`func (o *EnumTest) SetEnumString(v string)`

SetEnumString gets a reference to the given string and assigns it to the EnumString field.

### GetEnumStringRequired

`func (o *EnumTest) GetEnumStringRequired() string`

GetEnumStringRequired returns the EnumStringRequired field if non-nil, zero value otherwise.

### GetEnumStringRequiredOk

`func (o *EnumTest) GetEnumStringRequiredOk() (string, bool)`

GetEnumStringRequiredOk returns a tuple with the EnumStringRequired field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasEnumStringRequired

`func (o *EnumTest) HasEnumStringRequired() bool`

HasEnumStringRequired returns a boolean if a field has been set.

### SetEnumStringRequired

`func (o *EnumTest) SetEnumStringRequired(v string)`

SetEnumStringRequired gets a reference to the given string and assigns it to the EnumStringRequired field.

### GetEnumInteger

`func (o *EnumTest) GetEnumInteger() int32`

GetEnumInteger returns the EnumInteger field if non-nil, zero value otherwise.

### GetEnumIntegerOk

`func (o *EnumTest) GetEnumIntegerOk() (int32, bool)`

GetEnumIntegerOk returns a tuple with the EnumInteger field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasEnumInteger

`func (o *EnumTest) HasEnumInteger() bool`

HasEnumInteger returns a boolean if a field has been set.

### SetEnumInteger

`func (o *EnumTest) SetEnumInteger(v int32)`

SetEnumInteger gets a reference to the given int32 and assigns it to the EnumInteger field.

### GetEnumNumber

`func (o *EnumTest) GetEnumNumber() float64`

GetEnumNumber returns the EnumNumber field if non-nil, zero value otherwise.

### GetEnumNumberOk

`func (o *EnumTest) GetEnumNumberOk() (float64, bool)`

GetEnumNumberOk returns a tuple with the EnumNumber field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasEnumNumber

`func (o *EnumTest) HasEnumNumber() bool`

HasEnumNumber returns a boolean if a field has been set.

### SetEnumNumber

`func (o *EnumTest) SetEnumNumber(v float64)`

SetEnumNumber gets a reference to the given float64 and assigns it to the EnumNumber field.

### GetOuterEnum

`func (o *EnumTest) GetOuterEnum() NullableOuterEnum`

GetOuterEnum returns the OuterEnum field if non-nil, zero value otherwise.

### GetOuterEnumOk

`func (o *EnumTest) GetOuterEnumOk() (NullableOuterEnum, bool)`

GetOuterEnumOk returns a tuple with the OuterEnum field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasOuterEnum

`func (o *EnumTest) HasOuterEnum() bool`

HasOuterEnum returns a boolean if a field has been set.

### SetOuterEnum

`func (o *EnumTest) SetOuterEnum(v NullableOuterEnum)`

SetOuterEnum gets a reference to the given NullableOuterEnum and assigns it to the OuterEnum field.

### SetOuterEnumExplicitNull

`func (o *EnumTest) SetOuterEnumExplicitNull(b bool)`

SetOuterEnumExplicitNull (un)sets OuterEnum to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The OuterEnum value is set to nil even if false is passed
### GetOuterEnumInteger

`func (o *EnumTest) GetOuterEnumInteger() OuterEnumInteger`

GetOuterEnumInteger returns the OuterEnumInteger field if non-nil, zero value otherwise.

### GetOuterEnumIntegerOk

`func (o *EnumTest) GetOuterEnumIntegerOk() (OuterEnumInteger, bool)`

GetOuterEnumIntegerOk returns a tuple with the OuterEnumInteger field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasOuterEnumInteger

`func (o *EnumTest) HasOuterEnumInteger() bool`

HasOuterEnumInteger returns a boolean if a field has been set.

### SetOuterEnumInteger

`func (o *EnumTest) SetOuterEnumInteger(v OuterEnumInteger)`

SetOuterEnumInteger gets a reference to the given OuterEnumInteger and assigns it to the OuterEnumInteger field.

### GetOuterEnumDefaultValue

`func (o *EnumTest) GetOuterEnumDefaultValue() OuterEnumDefaultValue`

GetOuterEnumDefaultValue returns the OuterEnumDefaultValue field if non-nil, zero value otherwise.

### GetOuterEnumDefaultValueOk

`func (o *EnumTest) GetOuterEnumDefaultValueOk() (OuterEnumDefaultValue, bool)`

GetOuterEnumDefaultValueOk returns a tuple with the OuterEnumDefaultValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasOuterEnumDefaultValue

`func (o *EnumTest) HasOuterEnumDefaultValue() bool`

HasOuterEnumDefaultValue returns a boolean if a field has been set.

### SetOuterEnumDefaultValue

`func (o *EnumTest) SetOuterEnumDefaultValue(v OuterEnumDefaultValue)`

SetOuterEnumDefaultValue gets a reference to the given OuterEnumDefaultValue and assigns it to the OuterEnumDefaultValue field.

### GetOuterEnumIntegerDefaultValue

`func (o *EnumTest) GetOuterEnumIntegerDefaultValue() OuterEnumIntegerDefaultValue`

GetOuterEnumIntegerDefaultValue returns the OuterEnumIntegerDefaultValue field if non-nil, zero value otherwise.

### GetOuterEnumIntegerDefaultValueOk

`func (o *EnumTest) GetOuterEnumIntegerDefaultValueOk() (OuterEnumIntegerDefaultValue, bool)`

GetOuterEnumIntegerDefaultValueOk returns a tuple with the OuterEnumIntegerDefaultValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasOuterEnumIntegerDefaultValue

`func (o *EnumTest) HasOuterEnumIntegerDefaultValue() bool`

HasOuterEnumIntegerDefaultValue returns a boolean if a field has been set.

### SetOuterEnumIntegerDefaultValue

`func (o *EnumTest) SetOuterEnumIntegerDefaultValue(v OuterEnumIntegerDefaultValue)`

SetOuterEnumIntegerDefaultValue gets a reference to the given OuterEnumIntegerDefaultValue and assigns it to the OuterEnumIntegerDefaultValue field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


