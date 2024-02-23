# DefaultValue

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ArrayStringEnumRefDefault** | Pointer to [**[]StringEnumRef**](StringEnumRef.md) |  | [optional] [default to ["success","failure"]]
**ArrayStringEnumDefault** | Pointer to **[]string** |  | [optional] [default to ["success","failure"]]
**ArrayStringDefault** | Pointer to **[]string** |  | [optional] [default to ["failure","skipped"]]
**ArrayIntegerDefault** | Pointer to **[]int32** |  | [optional] [default to [1,3]]
**ArrayString** | Pointer to **[]string** |  | [optional] 
**ArrayStringNullable** | Pointer to **[]string** |  | [optional] 
**ArrayStringExtensionNullable** | Pointer to **[]string** |  | [optional] 
**StringNullable** | Pointer to **NullableString** |  | [optional] 

## Methods

### NewDefaultValue

`func NewDefaultValue() *DefaultValue`

NewDefaultValue instantiates a new DefaultValue object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewDefaultValueWithDefaults

`func NewDefaultValueWithDefaults() *DefaultValue`

NewDefaultValueWithDefaults instantiates a new DefaultValue object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetArrayStringEnumRefDefault

`func (o *DefaultValue) GetArrayStringEnumRefDefault() []StringEnumRef`

GetArrayStringEnumRefDefault returns the ArrayStringEnumRefDefault field if non-nil, zero value otherwise.

### GetArrayStringEnumRefDefaultOk

`func (o *DefaultValue) GetArrayStringEnumRefDefaultOk() (*[]StringEnumRef, bool)`

GetArrayStringEnumRefDefaultOk returns a tuple with the ArrayStringEnumRefDefault field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayStringEnumRefDefault

`func (o *DefaultValue) SetArrayStringEnumRefDefault(v []StringEnumRef)`

SetArrayStringEnumRefDefault sets ArrayStringEnumRefDefault field to given value.

### HasArrayStringEnumRefDefault

`func (o *DefaultValue) HasArrayStringEnumRefDefault() bool`

HasArrayStringEnumRefDefault returns a boolean if a field has been set.

### GetArrayStringEnumDefault

`func (o *DefaultValue) GetArrayStringEnumDefault() []string`

GetArrayStringEnumDefault returns the ArrayStringEnumDefault field if non-nil, zero value otherwise.

### GetArrayStringEnumDefaultOk

`func (o *DefaultValue) GetArrayStringEnumDefaultOk() (*[]string, bool)`

GetArrayStringEnumDefaultOk returns a tuple with the ArrayStringEnumDefault field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayStringEnumDefault

`func (o *DefaultValue) SetArrayStringEnumDefault(v []string)`

SetArrayStringEnumDefault sets ArrayStringEnumDefault field to given value.

### HasArrayStringEnumDefault

`func (o *DefaultValue) HasArrayStringEnumDefault() bool`

HasArrayStringEnumDefault returns a boolean if a field has been set.

### GetArrayStringDefault

`func (o *DefaultValue) GetArrayStringDefault() []string`

GetArrayStringDefault returns the ArrayStringDefault field if non-nil, zero value otherwise.

### GetArrayStringDefaultOk

`func (o *DefaultValue) GetArrayStringDefaultOk() (*[]string, bool)`

GetArrayStringDefaultOk returns a tuple with the ArrayStringDefault field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayStringDefault

`func (o *DefaultValue) SetArrayStringDefault(v []string)`

SetArrayStringDefault sets ArrayStringDefault field to given value.

### HasArrayStringDefault

`func (o *DefaultValue) HasArrayStringDefault() bool`

HasArrayStringDefault returns a boolean if a field has been set.

### GetArrayIntegerDefault

`func (o *DefaultValue) GetArrayIntegerDefault() []int32`

GetArrayIntegerDefault returns the ArrayIntegerDefault field if non-nil, zero value otherwise.

### GetArrayIntegerDefaultOk

`func (o *DefaultValue) GetArrayIntegerDefaultOk() (*[]int32, bool)`

GetArrayIntegerDefaultOk returns a tuple with the ArrayIntegerDefault field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayIntegerDefault

`func (o *DefaultValue) SetArrayIntegerDefault(v []int32)`

SetArrayIntegerDefault sets ArrayIntegerDefault field to given value.

### HasArrayIntegerDefault

`func (o *DefaultValue) HasArrayIntegerDefault() bool`

HasArrayIntegerDefault returns a boolean if a field has been set.

### GetArrayString

`func (o *DefaultValue) GetArrayString() []string`

GetArrayString returns the ArrayString field if non-nil, zero value otherwise.

### GetArrayStringOk

`func (o *DefaultValue) GetArrayStringOk() (*[]string, bool)`

GetArrayStringOk returns a tuple with the ArrayString field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayString

`func (o *DefaultValue) SetArrayString(v []string)`

SetArrayString sets ArrayString field to given value.

### HasArrayString

`func (o *DefaultValue) HasArrayString() bool`

HasArrayString returns a boolean if a field has been set.

### GetArrayStringNullable

`func (o *DefaultValue) GetArrayStringNullable() []string`

GetArrayStringNullable returns the ArrayStringNullable field if non-nil, zero value otherwise.

### GetArrayStringNullableOk

`func (o *DefaultValue) GetArrayStringNullableOk() (*[]string, bool)`

GetArrayStringNullableOk returns a tuple with the ArrayStringNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayStringNullable

`func (o *DefaultValue) SetArrayStringNullable(v []string)`

SetArrayStringNullable sets ArrayStringNullable field to given value.

### HasArrayStringNullable

`func (o *DefaultValue) HasArrayStringNullable() bool`

HasArrayStringNullable returns a boolean if a field has been set.

### SetArrayStringNullableNil

`func (o *DefaultValue) SetArrayStringNullableNil(b bool)`

 SetArrayStringNullableNil sets the value for ArrayStringNullable to be an explicit nil

### UnsetArrayStringNullable
`func (o *DefaultValue) UnsetArrayStringNullable()`

UnsetArrayStringNullable ensures that no value is present for ArrayStringNullable, not even an explicit nil
### GetArrayStringExtensionNullable

`func (o *DefaultValue) GetArrayStringExtensionNullable() []string`

GetArrayStringExtensionNullable returns the ArrayStringExtensionNullable field if non-nil, zero value otherwise.

### GetArrayStringExtensionNullableOk

`func (o *DefaultValue) GetArrayStringExtensionNullableOk() (*[]string, bool)`

GetArrayStringExtensionNullableOk returns a tuple with the ArrayStringExtensionNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayStringExtensionNullable

`func (o *DefaultValue) SetArrayStringExtensionNullable(v []string)`

SetArrayStringExtensionNullable sets ArrayStringExtensionNullable field to given value.

### HasArrayStringExtensionNullable

`func (o *DefaultValue) HasArrayStringExtensionNullable() bool`

HasArrayStringExtensionNullable returns a boolean if a field has been set.

### SetArrayStringExtensionNullableNil

`func (o *DefaultValue) SetArrayStringExtensionNullableNil(b bool)`

 SetArrayStringExtensionNullableNil sets the value for ArrayStringExtensionNullable to be an explicit nil

### UnsetArrayStringExtensionNullable
`func (o *DefaultValue) UnsetArrayStringExtensionNullable()`

UnsetArrayStringExtensionNullable ensures that no value is present for ArrayStringExtensionNullable, not even an explicit nil
### GetStringNullable

`func (o *DefaultValue) GetStringNullable() string`

GetStringNullable returns the StringNullable field if non-nil, zero value otherwise.

### GetStringNullableOk

`func (o *DefaultValue) GetStringNullableOk() (*string, bool)`

GetStringNullableOk returns a tuple with the StringNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStringNullable

`func (o *DefaultValue) SetStringNullable(v string)`

SetStringNullable sets StringNullable field to given value.

### HasStringNullable

`func (o *DefaultValue) HasStringNullable() bool`

HasStringNullable returns a boolean if a field has been set.

### SetStringNullableNil

`func (o *DefaultValue) SetStringNullableNil(b bool)`

 SetStringNullableNil sets the value for StringNullable to be an explicit nil

### UnsetStringNullable
`func (o *DefaultValue) UnsetStringNullable()`

UnsetStringNullable ensures that no value is present for StringNullable, not even an explicit nil

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


