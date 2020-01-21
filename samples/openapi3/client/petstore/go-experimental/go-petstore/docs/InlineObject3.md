# InlineObject3

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Integer** | Pointer to **int32** | None | [optional] 
**Int32** | Pointer to **int32** | None | [optional] 
**Int64** | Pointer to **int64** | None | [optional] 
**Number** | Pointer to **float32** | None | 
**Float** | Pointer to **float32** | None | [optional] 
**Double** | Pointer to **float64** | None | 
**String** | Pointer to **string** | None | [optional] 
**PatternWithoutDelimiter** | Pointer to **string** | None | 
**Byte** | Pointer to **string** | None | 
**Binary** | Pointer to [***os.File**](*os.File.md) | None | [optional] 
**Date** | Pointer to **string** | None | [optional] 
**DateTime** | Pointer to [**time.Time**](time.Time.md) | None | [optional] 
**Password** | Pointer to **string** | None | [optional] 
**Callback** | Pointer to **string** | None | [optional] 

## Methods

### GetInteger

`func (o *InlineObject3) GetInteger() int32`

GetInteger returns the Integer field if non-nil, zero value otherwise.

### GetIntegerOk

`func (o *InlineObject3) GetIntegerOk() (int32, bool)`

GetIntegerOk returns a tuple with the Integer field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasInteger

`func (o *InlineObject3) HasInteger() bool`

HasInteger returns a boolean if a field has been set.

### SetInteger

`func (o *InlineObject3) SetInteger(v int32)`

SetInteger gets a reference to the given int32 and assigns it to the Integer field.

### GetInt32

`func (o *InlineObject3) GetInt32() int32`

GetInt32 returns the Int32 field if non-nil, zero value otherwise.

### GetInt32Ok

`func (o *InlineObject3) GetInt32Ok() (int32, bool)`

GetInt32Ok returns a tuple with the Int32 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasInt32

`func (o *InlineObject3) HasInt32() bool`

HasInt32 returns a boolean if a field has been set.

### SetInt32

`func (o *InlineObject3) SetInt32(v int32)`

SetInt32 gets a reference to the given int32 and assigns it to the Int32 field.

### GetInt64

`func (o *InlineObject3) GetInt64() int64`

GetInt64 returns the Int64 field if non-nil, zero value otherwise.

### GetInt64Ok

`func (o *InlineObject3) GetInt64Ok() (int64, bool)`

GetInt64Ok returns a tuple with the Int64 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasInt64

`func (o *InlineObject3) HasInt64() bool`

HasInt64 returns a boolean if a field has been set.

### SetInt64

`func (o *InlineObject3) SetInt64(v int64)`

SetInt64 gets a reference to the given int64 and assigns it to the Int64 field.

### GetNumber

`func (o *InlineObject3) GetNumber() float32`

GetNumber returns the Number field if non-nil, zero value otherwise.

### GetNumberOk

`func (o *InlineObject3) GetNumberOk() (float32, bool)`

GetNumberOk returns a tuple with the Number field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasNumber

`func (o *InlineObject3) HasNumber() bool`

HasNumber returns a boolean if a field has been set.

### SetNumber

`func (o *InlineObject3) SetNumber(v float32)`

SetNumber gets a reference to the given float32 and assigns it to the Number field.

### GetFloat

`func (o *InlineObject3) GetFloat() float32`

GetFloat returns the Float field if non-nil, zero value otherwise.

### GetFloatOk

`func (o *InlineObject3) GetFloatOk() (float32, bool)`

GetFloatOk returns a tuple with the Float field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasFloat

`func (o *InlineObject3) HasFloat() bool`

HasFloat returns a boolean if a field has been set.

### SetFloat

`func (o *InlineObject3) SetFloat(v float32)`

SetFloat gets a reference to the given float32 and assigns it to the Float field.

### GetDouble

`func (o *InlineObject3) GetDouble() float64`

GetDouble returns the Double field if non-nil, zero value otherwise.

### GetDoubleOk

`func (o *InlineObject3) GetDoubleOk() (float64, bool)`

GetDoubleOk returns a tuple with the Double field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasDouble

`func (o *InlineObject3) HasDouble() bool`

HasDouble returns a boolean if a field has been set.

### SetDouble

`func (o *InlineObject3) SetDouble(v float64)`

SetDouble gets a reference to the given float64 and assigns it to the Double field.

### GetString

`func (o *InlineObject3) GetString() string`

GetString returns the String field if non-nil, zero value otherwise.

### GetStringOk

`func (o *InlineObject3) GetStringOk() (string, bool)`

GetStringOk returns a tuple with the String field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasString

`func (o *InlineObject3) HasString() bool`

HasString returns a boolean if a field has been set.

### SetString

`func (o *InlineObject3) SetString(v string)`

SetString gets a reference to the given string and assigns it to the String field.

### GetPatternWithoutDelimiter

`func (o *InlineObject3) GetPatternWithoutDelimiter() string`

GetPatternWithoutDelimiter returns the PatternWithoutDelimiter field if non-nil, zero value otherwise.

### GetPatternWithoutDelimiterOk

`func (o *InlineObject3) GetPatternWithoutDelimiterOk() (string, bool)`

GetPatternWithoutDelimiterOk returns a tuple with the PatternWithoutDelimiter field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPatternWithoutDelimiter

`func (o *InlineObject3) HasPatternWithoutDelimiter() bool`

HasPatternWithoutDelimiter returns a boolean if a field has been set.

### SetPatternWithoutDelimiter

`func (o *InlineObject3) SetPatternWithoutDelimiter(v string)`

SetPatternWithoutDelimiter gets a reference to the given string and assigns it to the PatternWithoutDelimiter field.

### GetByte

`func (o *InlineObject3) GetByte() string`

GetByte returns the Byte field if non-nil, zero value otherwise.

### GetByteOk

`func (o *InlineObject3) GetByteOk() (string, bool)`

GetByteOk returns a tuple with the Byte field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasByte

`func (o *InlineObject3) HasByte() bool`

HasByte returns a boolean if a field has been set.

### SetByte

`func (o *InlineObject3) SetByte(v string)`

SetByte gets a reference to the given string and assigns it to the Byte field.

### GetBinary

`func (o *InlineObject3) GetBinary() *os.File`

GetBinary returns the Binary field if non-nil, zero value otherwise.

### GetBinaryOk

`func (o *InlineObject3) GetBinaryOk() (*os.File, bool)`

GetBinaryOk returns a tuple with the Binary field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasBinary

`func (o *InlineObject3) HasBinary() bool`

HasBinary returns a boolean if a field has been set.

### SetBinary

`func (o *InlineObject3) SetBinary(v *os.File)`

SetBinary gets a reference to the given *os.File and assigns it to the Binary field.

### GetDate

`func (o *InlineObject3) GetDate() string`

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *InlineObject3) GetDateOk() (string, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasDate

`func (o *InlineObject3) HasDate() bool`

HasDate returns a boolean if a field has been set.

### SetDate

`func (o *InlineObject3) SetDate(v string)`

SetDate gets a reference to the given string and assigns it to the Date field.

### GetDateTime

`func (o *InlineObject3) GetDateTime() time.Time`

GetDateTime returns the DateTime field if non-nil, zero value otherwise.

### GetDateTimeOk

`func (o *InlineObject3) GetDateTimeOk() (time.Time, bool)`

GetDateTimeOk returns a tuple with the DateTime field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasDateTime

`func (o *InlineObject3) HasDateTime() bool`

HasDateTime returns a boolean if a field has been set.

### SetDateTime

`func (o *InlineObject3) SetDateTime(v time.Time)`

SetDateTime gets a reference to the given time.Time and assigns it to the DateTime field.

### GetPassword

`func (o *InlineObject3) GetPassword() string`

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *InlineObject3) GetPasswordOk() (string, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPassword

`func (o *InlineObject3) HasPassword() bool`

HasPassword returns a boolean if a field has been set.

### SetPassword

`func (o *InlineObject3) SetPassword(v string)`

SetPassword gets a reference to the given string and assigns it to the Password field.

### GetCallback

`func (o *InlineObject3) GetCallback() string`

GetCallback returns the Callback field if non-nil, zero value otherwise.

### GetCallbackOk

`func (o *InlineObject3) GetCallbackOk() (string, bool)`

GetCallbackOk returns a tuple with the Callback field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasCallback

`func (o *InlineObject3) HasCallback() bool`

HasCallback returns a boolean if a field has been set.

### SetCallback

`func (o *InlineObject3) SetCallback(v string)`

SetCallback gets a reference to the given string and assigns it to the Callback field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


