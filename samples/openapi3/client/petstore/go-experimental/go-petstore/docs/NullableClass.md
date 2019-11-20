# NullableClass

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**IntegerProp** | Pointer to **NullableInt32** |  | [optional] 
**NumberProp** | Pointer to **NullableFloat32** |  | [optional] 
**BooleanProp** | Pointer to **NullableBool** |  | [optional] 
**StringProp** | Pointer to **NullableString** |  | [optional] 
**DateProp** | Pointer to **NullableString** |  | [optional] 
**DatetimeProp** | Pointer to [**NullableTime.Time**](time.Time.md) |  | [optional] 
**ArrayNullableProp** | Pointer to [**Nullable[]map[string]interface{}**](map[string]interface{}.md) |  | [optional] 
**ArrayAndItemsNullableProp** | Pointer to [**Nullable[]map[string]interface{}**](map[string]interface{}.md) |  | [optional] 
**ArrayItemsNullable** | Pointer to [**[]map[string]interface{}**](map[string]interface{}.md) |  | [optional] 
**ObjectNullableProp** | Pointer to [**NullableMap[string]map[string]interface{}**](map[string]interface{}.md) |  | [optional] 
**ObjectAndItemsNullableProp** | Pointer to [**NullableMap[string]map[string]interface{}**](map[string]interface{}.md) |  | [optional] 
**ObjectItemsNullable** | Pointer to [**map[string]map[string]interface{}**](map[string]interface{}.md) |  | [optional] 

## Methods

### GetIntegerProp

`func (o *NullableClass) GetIntegerProp() NullableInt32`

GetIntegerProp returns the IntegerProp field if non-nil, zero value otherwise.

### GetIntegerPropOk

`func (o *NullableClass) GetIntegerPropOk() (NullableInt32, bool)`

GetIntegerPropOk returns a tuple with the IntegerProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasIntegerProp

`func (o *NullableClass) HasIntegerProp() bool`

HasIntegerProp returns a boolean if a field has been set.

### SetIntegerProp

`func (o *NullableClass) SetIntegerProp(v NullableInt32)`

SetIntegerProp gets a reference to the given NullableInt32 and assigns it to the IntegerProp field.

### SetIntegerPropExplicitNull

`func (o *NullableClass) SetIntegerPropExplicitNull(b bool)`

SetIntegerPropExplicitNull (un)sets IntegerProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The IntegerProp value is set to nil even if false is passed
### GetNumberProp

`func (o *NullableClass) GetNumberProp() NullableFloat32`

GetNumberProp returns the NumberProp field if non-nil, zero value otherwise.

### GetNumberPropOk

`func (o *NullableClass) GetNumberPropOk() (NullableFloat32, bool)`

GetNumberPropOk returns a tuple with the NumberProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasNumberProp

`func (o *NullableClass) HasNumberProp() bool`

HasNumberProp returns a boolean if a field has been set.

### SetNumberProp

`func (o *NullableClass) SetNumberProp(v NullableFloat32)`

SetNumberProp gets a reference to the given NullableFloat32 and assigns it to the NumberProp field.

### SetNumberPropExplicitNull

`func (o *NullableClass) SetNumberPropExplicitNull(b bool)`

SetNumberPropExplicitNull (un)sets NumberProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The NumberProp value is set to nil even if false is passed
### GetBooleanProp

`func (o *NullableClass) GetBooleanProp() NullableBool`

GetBooleanProp returns the BooleanProp field if non-nil, zero value otherwise.

### GetBooleanPropOk

`func (o *NullableClass) GetBooleanPropOk() (NullableBool, bool)`

GetBooleanPropOk returns a tuple with the BooleanProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasBooleanProp

`func (o *NullableClass) HasBooleanProp() bool`

HasBooleanProp returns a boolean if a field has been set.

### SetBooleanProp

`func (o *NullableClass) SetBooleanProp(v NullableBool)`

SetBooleanProp gets a reference to the given NullableBool and assigns it to the BooleanProp field.

### SetBooleanPropExplicitNull

`func (o *NullableClass) SetBooleanPropExplicitNull(b bool)`

SetBooleanPropExplicitNull (un)sets BooleanProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The BooleanProp value is set to nil even if false is passed
### GetStringProp

`func (o *NullableClass) GetStringProp() NullableString`

GetStringProp returns the StringProp field if non-nil, zero value otherwise.

### GetStringPropOk

`func (o *NullableClass) GetStringPropOk() (NullableString, bool)`

GetStringPropOk returns a tuple with the StringProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasStringProp

`func (o *NullableClass) HasStringProp() bool`

HasStringProp returns a boolean if a field has been set.

### SetStringProp

`func (o *NullableClass) SetStringProp(v NullableString)`

SetStringProp gets a reference to the given NullableString and assigns it to the StringProp field.

### SetStringPropExplicitNull

`func (o *NullableClass) SetStringPropExplicitNull(b bool)`

SetStringPropExplicitNull (un)sets StringProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The StringProp value is set to nil even if false is passed
### GetDateProp

`func (o *NullableClass) GetDateProp() NullableString`

GetDateProp returns the DateProp field if non-nil, zero value otherwise.

### GetDatePropOk

`func (o *NullableClass) GetDatePropOk() (NullableString, bool)`

GetDatePropOk returns a tuple with the DateProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasDateProp

`func (o *NullableClass) HasDateProp() bool`

HasDateProp returns a boolean if a field has been set.

### SetDateProp

`func (o *NullableClass) SetDateProp(v NullableString)`

SetDateProp gets a reference to the given NullableString and assigns it to the DateProp field.

### SetDatePropExplicitNull

`func (o *NullableClass) SetDatePropExplicitNull(b bool)`

SetDatePropExplicitNull (un)sets DateProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The DateProp value is set to nil even if false is passed
### GetDatetimeProp

`func (o *NullableClass) GetDatetimeProp() NullableTime.Time`

GetDatetimeProp returns the DatetimeProp field if non-nil, zero value otherwise.

### GetDatetimePropOk

`func (o *NullableClass) GetDatetimePropOk() (NullableTime.Time, bool)`

GetDatetimePropOk returns a tuple with the DatetimeProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasDatetimeProp

`func (o *NullableClass) HasDatetimeProp() bool`

HasDatetimeProp returns a boolean if a field has been set.

### SetDatetimeProp

`func (o *NullableClass) SetDatetimeProp(v NullableTime.Time)`

SetDatetimeProp gets a reference to the given NullableTime.Time and assigns it to the DatetimeProp field.

### SetDatetimePropExplicitNull

`func (o *NullableClass) SetDatetimePropExplicitNull(b bool)`

SetDatetimePropExplicitNull (un)sets DatetimeProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The DatetimeProp value is set to nil even if false is passed
### GetArrayNullableProp

`func (o *NullableClass) GetArrayNullableProp() Nullable[]map[string]interface{}`

GetArrayNullableProp returns the ArrayNullableProp field if non-nil, zero value otherwise.

### GetArrayNullablePropOk

`func (o *NullableClass) GetArrayNullablePropOk() (Nullable[]map[string]interface{}, bool)`

GetArrayNullablePropOk returns a tuple with the ArrayNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArrayNullableProp

`func (o *NullableClass) HasArrayNullableProp() bool`

HasArrayNullableProp returns a boolean if a field has been set.

### SetArrayNullableProp

`func (o *NullableClass) SetArrayNullableProp(v Nullable[]map[string]interface{})`

SetArrayNullableProp gets a reference to the given Nullable[]map[string]interface{} and assigns it to the ArrayNullableProp field.

### SetArrayNullablePropExplicitNull

`func (o *NullableClass) SetArrayNullablePropExplicitNull(b bool)`

SetArrayNullablePropExplicitNull (un)sets ArrayNullableProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The ArrayNullableProp value is set to nil even if false is passed
### GetArrayAndItemsNullableProp

`func (o *NullableClass) GetArrayAndItemsNullableProp() Nullable[]map[string]interface{}`

GetArrayAndItemsNullableProp returns the ArrayAndItemsNullableProp field if non-nil, zero value otherwise.

### GetArrayAndItemsNullablePropOk

`func (o *NullableClass) GetArrayAndItemsNullablePropOk() (Nullable[]map[string]interface{}, bool)`

GetArrayAndItemsNullablePropOk returns a tuple with the ArrayAndItemsNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArrayAndItemsNullableProp

`func (o *NullableClass) HasArrayAndItemsNullableProp() bool`

HasArrayAndItemsNullableProp returns a boolean if a field has been set.

### SetArrayAndItemsNullableProp

`func (o *NullableClass) SetArrayAndItemsNullableProp(v Nullable[]map[string]interface{})`

SetArrayAndItemsNullableProp gets a reference to the given Nullable[]map[string]interface{} and assigns it to the ArrayAndItemsNullableProp field.

### SetArrayAndItemsNullablePropExplicitNull

`func (o *NullableClass) SetArrayAndItemsNullablePropExplicitNull(b bool)`

SetArrayAndItemsNullablePropExplicitNull (un)sets ArrayAndItemsNullableProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The ArrayAndItemsNullableProp value is set to nil even if false is passed
### GetArrayItemsNullable

`func (o *NullableClass) GetArrayItemsNullable() []map[string]interface{}`

GetArrayItemsNullable returns the ArrayItemsNullable field if non-nil, zero value otherwise.

### GetArrayItemsNullableOk

`func (o *NullableClass) GetArrayItemsNullableOk() ([]map[string]interface{}, bool)`

GetArrayItemsNullableOk returns a tuple with the ArrayItemsNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArrayItemsNullable

`func (o *NullableClass) HasArrayItemsNullable() bool`

HasArrayItemsNullable returns a boolean if a field has been set.

### SetArrayItemsNullable

`func (o *NullableClass) SetArrayItemsNullable(v []map[string]interface{})`

SetArrayItemsNullable gets a reference to the given []map[string]interface{} and assigns it to the ArrayItemsNullable field.

### GetObjectNullableProp

`func (o *NullableClass) GetObjectNullableProp() NullableMap[string]map[string]interface{}`

GetObjectNullableProp returns the ObjectNullableProp field if non-nil, zero value otherwise.

### GetObjectNullablePropOk

`func (o *NullableClass) GetObjectNullablePropOk() (NullableMap[string]map[string]interface{}, bool)`

GetObjectNullablePropOk returns a tuple with the ObjectNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasObjectNullableProp

`func (o *NullableClass) HasObjectNullableProp() bool`

HasObjectNullableProp returns a boolean if a field has been set.

### SetObjectNullableProp

`func (o *NullableClass) SetObjectNullableProp(v NullableMap[string]map[string]interface{})`

SetObjectNullableProp gets a reference to the given NullableMap[string]map[string]interface{} and assigns it to the ObjectNullableProp field.

### SetObjectNullablePropExplicitNull

`func (o *NullableClass) SetObjectNullablePropExplicitNull(b bool)`

SetObjectNullablePropExplicitNull (un)sets ObjectNullableProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The ObjectNullableProp value is set to nil even if false is passed
### GetObjectAndItemsNullableProp

`func (o *NullableClass) GetObjectAndItemsNullableProp() NullableMap[string]map[string]interface{}`

GetObjectAndItemsNullableProp returns the ObjectAndItemsNullableProp field if non-nil, zero value otherwise.

### GetObjectAndItemsNullablePropOk

`func (o *NullableClass) GetObjectAndItemsNullablePropOk() (NullableMap[string]map[string]interface{}, bool)`

GetObjectAndItemsNullablePropOk returns a tuple with the ObjectAndItemsNullableProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasObjectAndItemsNullableProp

`func (o *NullableClass) HasObjectAndItemsNullableProp() bool`

HasObjectAndItemsNullableProp returns a boolean if a field has been set.

### SetObjectAndItemsNullableProp

`func (o *NullableClass) SetObjectAndItemsNullableProp(v NullableMap[string]map[string]interface{})`

SetObjectAndItemsNullableProp gets a reference to the given NullableMap[string]map[string]interface{} and assigns it to the ObjectAndItemsNullableProp field.

### SetObjectAndItemsNullablePropExplicitNull

`func (o *NullableClass) SetObjectAndItemsNullablePropExplicitNull(b bool)`

SetObjectAndItemsNullablePropExplicitNull (un)sets ObjectAndItemsNullableProp to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The ObjectAndItemsNullableProp value is set to nil even if false is passed
### GetObjectItemsNullable

`func (o *NullableClass) GetObjectItemsNullable() map[string]map[string]interface{}`

GetObjectItemsNullable returns the ObjectItemsNullable field if non-nil, zero value otherwise.

### GetObjectItemsNullableOk

`func (o *NullableClass) GetObjectItemsNullableOk() (map[string]map[string]interface{}, bool)`

GetObjectItemsNullableOk returns a tuple with the ObjectItemsNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasObjectItemsNullable

`func (o *NullableClass) HasObjectItemsNullable() bool`

HasObjectItemsNullable returns a boolean if a field has been set.

### SetObjectItemsNullable

`func (o *NullableClass) SetObjectItemsNullable(v map[string]map[string]interface{})`

SetObjectItemsNullable gets a reference to the given map[string]map[string]interface{} and assigns it to the ObjectItemsNullable field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


