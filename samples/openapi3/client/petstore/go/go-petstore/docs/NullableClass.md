# NullableClass

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**IntegerProp** | Pointer to **NullableInt32** |  | [optional] 
**NumberProp** | Pointer to **NullableFloat32** |  | [optional] 
**BooleanProp** | Pointer to **NullableBool** |  | [optional] [default to false]
**StringProp** | Pointer to **NullableString** |  | [optional] 
**DateProp** | Pointer to **NullableString** |  | [optional] 
**DatetimeProp** | Pointer to **NullableTime** |  | [optional] 
**ArrayNullableProp** | Pointer to **[]map[string]interface{}** |  | [optional] 
**ArrayAndItemsNullableProp** | Pointer to **[]map[string]interface{}** |  | [optional] 
**ArrayItemsNullable** | Pointer to **[]map[string]interface{}** |  | [optional] 
**ObjectNullableProp** | Pointer to **map[string]map[string]interface{}** |  | [optional] 
**ObjectAndItemsNullableProp** | Pointer to **map[string]map[string]interface{}** |  | [optional] 
**ObjectItemsNullable** | Pointer to **map[string]map[string]interface{}** |  | [optional] 

## Methods

### NewNullableClass

`func NewNullableClass() *NullableClass`

NewNullableClass instantiates a new NullableClass object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewNullableClassWithDefaults

`func NewNullableClassWithDefaults() *NullableClass`

NewNullableClassWithDefaults instantiates a new NullableClass object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetIntegerProp

`func (o *NullableClass) GetIntegerProp() int32`

GetIntegerProp returns the IntegerProp field if non-nil, zero value otherwise.

### GetIntegerPropOk

`func (o *NullableClass) GetIntegerPropOk() (*int32, bool)`

GetIntegerPropOk returns a tuple with the IntegerProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetIntegerProp

`func (o *NullableClass) SetIntegerProp(v int32)`

SetIntegerProp sets IntegerProp field to given value.

### HasIntegerProp

`func (o *NullableClass) HasIntegerProp() bool`

HasIntegerProp returns a boolean if a field has been set.

### SetIntegerPropNil

`func (o *NullableClass) SetIntegerPropNil(b bool)`

 SetIntegerPropNil sets the value for IntegerProp to be an explicit nil

### UnsetIntegerProp
`func (o *NullableClass) UnsetIntegerProp()`

UnsetIntegerProp ensures that no value is present for IntegerProp, not even an explicit nil
### GetNumberProp

`func (o *NullableClass) GetNumberProp() float32`

GetNumberProp returns the NumberProp field if non-nil, zero value otherwise.

### GetNumberPropOk

`func (o *NullableClass) GetNumberPropOk() (*float32, bool)`

GetNumberPropOk returns a tuple with the NumberProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNumberProp

`func (o *NullableClass) SetNumberProp(v float32)`

SetNumberProp sets NumberProp field to given value.

### HasNumberProp

`func (o *NullableClass) HasNumberProp() bool`

HasNumberProp returns a boolean if a field has been set.

### SetNumberPropNil

`func (o *NullableClass) SetNumberPropNil(b bool)`

 SetNumberPropNil sets the value for NumberProp to be an explicit nil

### UnsetNumberProp
`func (o *NullableClass) UnsetNumberProp()`

UnsetNumberProp ensures that no value is present for NumberProp, not even an explicit nil
### GetBooleanProp

`func (o *NullableClass) GetBooleanProp() bool`

GetBooleanProp returns the BooleanProp field if non-nil, zero value otherwise.

### GetBooleanPropOk

`func (o *NullableClass) GetBooleanPropOk() (*bool, bool)`

GetBooleanPropOk returns a tuple with the BooleanProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBooleanProp

`func (o *NullableClass) SetBooleanProp(v bool)`

SetBooleanProp sets BooleanProp field to given value.

### HasBooleanProp

`func (o *NullableClass) HasBooleanProp() bool`

HasBooleanProp returns a boolean if a field has been set.

### SetBooleanPropNil

`func (o *NullableClass) SetBooleanPropNil(b bool)`

 SetBooleanPropNil sets the value for BooleanProp to be an explicit nil

### UnsetBooleanProp
`func (o *NullableClass) UnsetBooleanProp()`

UnsetBooleanProp ensures that no value is present for BooleanProp, not even an explicit nil
### GetStringProp

`func (o *NullableClass) GetStringProp() string`

GetStringProp returns the StringProp field if non-nil, zero value otherwise.

### GetStringPropOk

`func (o *NullableClass) GetStringPropOk() (*string, bool)`

GetStringPropOk returns a tuple with the StringProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStringProp

`func (o *NullableClass) SetStringProp(v string)`

SetStringProp sets StringProp field to given value.

### HasStringProp

`func (o *NullableClass) HasStringProp() bool`

HasStringProp returns a boolean if a field has been set.

### SetStringPropNil

`func (o *NullableClass) SetStringPropNil(b bool)`

 SetStringPropNil sets the value for StringProp to be an explicit nil

### UnsetStringProp
`func (o *NullableClass) UnsetStringProp()`

UnsetStringProp ensures that no value is present for StringProp, not even an explicit nil
### GetDateProp

`func (o *NullableClass) GetDateProp() string`

GetDateProp returns the DateProp field if non-nil, zero value otherwise.

### GetDatePropOk

`func (o *NullableClass) GetDatePropOk() (*string, bool)`

GetDatePropOk returns a tuple with the DateProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDateProp

`func (o *NullableClass) SetDateProp(v string)`

SetDateProp sets DateProp field to given value.

### HasDateProp

`func (o *NullableClass) HasDateProp() bool`

HasDateProp returns a boolean if a field has been set.

### SetDatePropNil

`func (o *NullableClass) SetDatePropNil(b bool)`

 SetDatePropNil sets the value for DateProp to be an explicit nil

### UnsetDateProp
`func (o *NullableClass) UnsetDateProp()`

UnsetDateProp ensures that no value is present for DateProp, not even an explicit nil
### GetDatetimeProp

`func (o *NullableClass) GetDatetimeProp() time.Time`

GetDatetimeProp returns the DatetimeProp field if non-nil, zero value otherwise.

### GetDatetimePropOk

`func (o *NullableClass) GetDatetimePropOk() (*time.Time, bool)`

GetDatetimePropOk returns a tuple with the DatetimeProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDatetimeProp

`func (o *NullableClass) SetDatetimeProp(v time.Time)`

SetDatetimeProp sets DatetimeProp field to given value.

### HasDatetimeProp

`func (o *NullableClass) HasDatetimeProp() bool`

HasDatetimeProp returns a boolean if a field has been set.

### SetDatetimePropNil

`func (o *NullableClass) SetDatetimePropNil(b bool)`

 SetDatetimePropNil sets the value for DatetimeProp to be an explicit nil

### UnsetDatetimeProp
`func (o *NullableClass) UnsetDatetimeProp()`

UnsetDatetimeProp ensures that no value is present for DatetimeProp, not even an explicit nil
### GetArrayNullableProp

`func (o *NullableClass) GetArrayNullableProp() []map[string]interface{}`

GetArrayNullableProp returns the ArrayNullableProp field if non-nil, zero value otherwise.

### GetArrayNullablePropOk

`func (o *NullableClass) GetArrayNullablePropOk() (*[]map[string]interface{}, bool)`

GetArrayNullablePropOk returns a tuple with the ArrayNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayNullableProp

`func (o *NullableClass) SetArrayNullableProp(v []map[string]interface{})`

SetArrayNullableProp sets ArrayNullableProp field to given value.

### HasArrayNullableProp

`func (o *NullableClass) HasArrayNullableProp() bool`

HasArrayNullableProp returns a boolean if a field has been set.

### SetArrayNullablePropNil

`func (o *NullableClass) SetArrayNullablePropNil(b bool)`

 SetArrayNullablePropNil sets the value for ArrayNullableProp to be an explicit nil

### UnsetArrayNullableProp
`func (o *NullableClass) UnsetArrayNullableProp()`

UnsetArrayNullableProp ensures that no value is present for ArrayNullableProp, not even an explicit nil
### GetArrayAndItemsNullableProp

`func (o *NullableClass) GetArrayAndItemsNullableProp() []*map[string]interface{}`

GetArrayAndItemsNullableProp returns the ArrayAndItemsNullableProp field if non-nil, zero value otherwise.

### GetArrayAndItemsNullablePropOk

`func (o *NullableClass) GetArrayAndItemsNullablePropOk() (*[]*map[string]interface{}, bool)`

GetArrayAndItemsNullablePropOk returns a tuple with the ArrayAndItemsNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayAndItemsNullableProp

`func (o *NullableClass) SetArrayAndItemsNullableProp(v []*map[string]interface{})`

SetArrayAndItemsNullableProp sets ArrayAndItemsNullableProp field to given value.

### HasArrayAndItemsNullableProp

`func (o *NullableClass) HasArrayAndItemsNullableProp() bool`

HasArrayAndItemsNullableProp returns a boolean if a field has been set.

### SetArrayAndItemsNullablePropNil

`func (o *NullableClass) SetArrayAndItemsNullablePropNil(b bool)`

 SetArrayAndItemsNullablePropNil sets the value for ArrayAndItemsNullableProp to be an explicit nil

### UnsetArrayAndItemsNullableProp
`func (o *NullableClass) UnsetArrayAndItemsNullableProp()`

UnsetArrayAndItemsNullableProp ensures that no value is present for ArrayAndItemsNullableProp, not even an explicit nil
### GetArrayItemsNullable

`func (o *NullableClass) GetArrayItemsNullable() []*map[string]interface{}`

GetArrayItemsNullable returns the ArrayItemsNullable field if non-nil, zero value otherwise.

### GetArrayItemsNullableOk

`func (o *NullableClass) GetArrayItemsNullableOk() (*[]*map[string]interface{}, bool)`

GetArrayItemsNullableOk returns a tuple with the ArrayItemsNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArrayItemsNullable

`func (o *NullableClass) SetArrayItemsNullable(v []*map[string]interface{})`

SetArrayItemsNullable sets ArrayItemsNullable field to given value.

### HasArrayItemsNullable

`func (o *NullableClass) HasArrayItemsNullable() bool`

HasArrayItemsNullable returns a boolean if a field has been set.

### GetObjectNullableProp

`func (o *NullableClass) GetObjectNullableProp() map[string]map[string]interface{}`

GetObjectNullableProp returns the ObjectNullableProp field if non-nil, zero value otherwise.

### GetObjectNullablePropOk

`func (o *NullableClass) GetObjectNullablePropOk() (*map[string]map[string]interface{}, bool)`

GetObjectNullablePropOk returns a tuple with the ObjectNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetObjectNullableProp

`func (o *NullableClass) SetObjectNullableProp(v map[string]map[string]interface{})`

SetObjectNullableProp sets ObjectNullableProp field to given value.

### HasObjectNullableProp

`func (o *NullableClass) HasObjectNullableProp() bool`

HasObjectNullableProp returns a boolean if a field has been set.

### SetObjectNullablePropNil

`func (o *NullableClass) SetObjectNullablePropNil(b bool)`

 SetObjectNullablePropNil sets the value for ObjectNullableProp to be an explicit nil

### UnsetObjectNullableProp
`func (o *NullableClass) UnsetObjectNullableProp()`

UnsetObjectNullableProp ensures that no value is present for ObjectNullableProp, not even an explicit nil
### GetObjectAndItemsNullableProp

`func (o *NullableClass) GetObjectAndItemsNullableProp() map[string]map[string]interface{}`

GetObjectAndItemsNullableProp returns the ObjectAndItemsNullableProp field if non-nil, zero value otherwise.

### GetObjectAndItemsNullablePropOk

`func (o *NullableClass) GetObjectAndItemsNullablePropOk() (*map[string]map[string]interface{}, bool)`

GetObjectAndItemsNullablePropOk returns a tuple with the ObjectAndItemsNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetObjectAndItemsNullableProp

`func (o *NullableClass) SetObjectAndItemsNullableProp(v map[string]map[string]interface{})`

SetObjectAndItemsNullableProp sets ObjectAndItemsNullableProp field to given value.

### HasObjectAndItemsNullableProp

`func (o *NullableClass) HasObjectAndItemsNullableProp() bool`

HasObjectAndItemsNullableProp returns a boolean if a field has been set.

### SetObjectAndItemsNullablePropNil

`func (o *NullableClass) SetObjectAndItemsNullablePropNil(b bool)`

 SetObjectAndItemsNullablePropNil sets the value for ObjectAndItemsNullableProp to be an explicit nil

### UnsetObjectAndItemsNullableProp
`func (o *NullableClass) UnsetObjectAndItemsNullableProp()`

UnsetObjectAndItemsNullableProp ensures that no value is present for ObjectAndItemsNullableProp, not even an explicit nil
### GetObjectItemsNullable

`func (o *NullableClass) GetObjectItemsNullable() map[string]map[string]interface{}`

GetObjectItemsNullable returns the ObjectItemsNullable field if non-nil, zero value otherwise.

### GetObjectItemsNullableOk

`func (o *NullableClass) GetObjectItemsNullableOk() (*map[string]map[string]interface{}, bool)`

GetObjectItemsNullableOk returns a tuple with the ObjectItemsNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetObjectItemsNullable

`func (o *NullableClass) SetObjectItemsNullable(v map[string]map[string]interface{})`

SetObjectItemsNullable sets ObjectItemsNullable field to given value.

### HasObjectItemsNullable

`func (o *NullableClass) HasObjectItemsNullable() bool`

HasObjectItemsNullable returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


