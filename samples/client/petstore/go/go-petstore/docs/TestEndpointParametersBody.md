# TestEndpointParametersBody

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Integer** | Pointer to **int32** | None | [optional] 
**Int32** | Pointer to **int32** | None | [optional] 
**Int64** | Pointer to **int64** | None | [optional] 
**Number** | **float32** | None | 
**Float** | Pointer to **float32** | None | [optional] 
**Double** | **float64** | None | 
**String** | Pointer to **string** | None | [optional] 
**PatternWithoutDelimiter** | **string** | None | 
**Byte** | **string** | None | 
**Binary** | Pointer to ***os.File** | None | [optional] 
**Date** | Pointer to **string** | None | [optional] 
**DateTime** | Pointer to **time.Time** | None | [optional] 
**Password** | Pointer to **string** | None | [optional] 
**Callback** | Pointer to **string** | None | [optional] 

## Methods

### NewTestEndpointParametersBody

`func NewTestEndpointParametersBody(number float32, double float64, patternWithoutDelimiter string, byte_ string, ) *TestEndpointParametersBody`

NewTestEndpointParametersBody instantiates a new TestEndpointParametersBody object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewTestEndpointParametersBodyWithDefaults

`func NewTestEndpointParametersBodyWithDefaults() *TestEndpointParametersBody`

NewTestEndpointParametersBodyWithDefaults instantiates a new TestEndpointParametersBody object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetInteger

`func (o *TestEndpointParametersBody) GetInteger() int32`

GetInteger returns the Integer field if non-nil, zero value otherwise.

### GetIntegerOk

`func (o *TestEndpointParametersBody) GetIntegerOk() (*int32, bool)`

GetIntegerOk returns a tuple with the Integer field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInteger

`func (o *TestEndpointParametersBody) SetInteger(v int32)`

SetInteger sets Integer field to given value.

### HasInteger

`func (o *TestEndpointParametersBody) HasInteger() bool`

HasInteger returns a boolean if a field has been set.

### GetInt32

`func (o *TestEndpointParametersBody) GetInt32() int32`

GetInt32 returns the Int32 field if non-nil, zero value otherwise.

### GetInt32Ok

`func (o *TestEndpointParametersBody) GetInt32Ok() (*int32, bool)`

GetInt32Ok returns a tuple with the Int32 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt32

`func (o *TestEndpointParametersBody) SetInt32(v int32)`

SetInt32 sets Int32 field to given value.

### HasInt32

`func (o *TestEndpointParametersBody) HasInt32() bool`

HasInt32 returns a boolean if a field has been set.

### GetInt64

`func (o *TestEndpointParametersBody) GetInt64() int64`

GetInt64 returns the Int64 field if non-nil, zero value otherwise.

### GetInt64Ok

`func (o *TestEndpointParametersBody) GetInt64Ok() (*int64, bool)`

GetInt64Ok returns a tuple with the Int64 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt64

`func (o *TestEndpointParametersBody) SetInt64(v int64)`

SetInt64 sets Int64 field to given value.

### HasInt64

`func (o *TestEndpointParametersBody) HasInt64() bool`

HasInt64 returns a boolean if a field has been set.

### GetNumber

`func (o *TestEndpointParametersBody) GetNumber() float32`

GetNumber returns the Number field if non-nil, zero value otherwise.

### GetNumberOk

`func (o *TestEndpointParametersBody) GetNumberOk() (*float32, bool)`

GetNumberOk returns a tuple with the Number field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNumber

`func (o *TestEndpointParametersBody) SetNumber(v float32)`

SetNumber sets Number field to given value.


### GetFloat

`func (o *TestEndpointParametersBody) GetFloat() float32`

GetFloat returns the Float field if non-nil, zero value otherwise.

### GetFloatOk

`func (o *TestEndpointParametersBody) GetFloatOk() (*float32, bool)`

GetFloatOk returns a tuple with the Float field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFloat

`func (o *TestEndpointParametersBody) SetFloat(v float32)`

SetFloat sets Float field to given value.

### HasFloat

`func (o *TestEndpointParametersBody) HasFloat() bool`

HasFloat returns a boolean if a field has been set.

### GetDouble

`func (o *TestEndpointParametersBody) GetDouble() float64`

GetDouble returns the Double field if non-nil, zero value otherwise.

### GetDoubleOk

`func (o *TestEndpointParametersBody) GetDoubleOk() (*float64, bool)`

GetDoubleOk returns a tuple with the Double field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDouble

`func (o *TestEndpointParametersBody) SetDouble(v float64)`

SetDouble sets Double field to given value.


### GetString

`func (o *TestEndpointParametersBody) GetString() string`

GetString returns the String field if non-nil, zero value otherwise.

### GetStringOk

`func (o *TestEndpointParametersBody) GetStringOk() (*string, bool)`

GetStringOk returns a tuple with the String field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetString

`func (o *TestEndpointParametersBody) SetString(v string)`

SetString sets String field to given value.

### HasString

`func (o *TestEndpointParametersBody) HasString() bool`

HasString returns a boolean if a field has been set.

### GetPatternWithoutDelimiter

`func (o *TestEndpointParametersBody) GetPatternWithoutDelimiter() string`

GetPatternWithoutDelimiter returns the PatternWithoutDelimiter field if non-nil, zero value otherwise.

### GetPatternWithoutDelimiterOk

`func (o *TestEndpointParametersBody) GetPatternWithoutDelimiterOk() (*string, bool)`

GetPatternWithoutDelimiterOk returns a tuple with the PatternWithoutDelimiter field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPatternWithoutDelimiter

`func (o *TestEndpointParametersBody) SetPatternWithoutDelimiter(v string)`

SetPatternWithoutDelimiter sets PatternWithoutDelimiter field to given value.


### GetByte

`func (o *TestEndpointParametersBody) GetByte() string`

GetByte returns the Byte field if non-nil, zero value otherwise.

### GetByteOk

`func (o *TestEndpointParametersBody) GetByteOk() (*string, bool)`

GetByteOk returns a tuple with the Byte field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetByte

`func (o *TestEndpointParametersBody) SetByte(v string)`

SetByte sets Byte field to given value.


### GetBinary

`func (o *TestEndpointParametersBody) GetBinary() *os.File`

GetBinary returns the Binary field if non-nil, zero value otherwise.

### GetBinaryOk

`func (o *TestEndpointParametersBody) GetBinaryOk() (**os.File, bool)`

GetBinaryOk returns a tuple with the Binary field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBinary

`func (o *TestEndpointParametersBody) SetBinary(v *os.File)`

SetBinary sets Binary field to given value.

### HasBinary

`func (o *TestEndpointParametersBody) HasBinary() bool`

HasBinary returns a boolean if a field has been set.

### GetDate

`func (o *TestEndpointParametersBody) GetDate() string`

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *TestEndpointParametersBody) GetDateOk() (*string, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *TestEndpointParametersBody) SetDate(v string)`

SetDate sets Date field to given value.

### HasDate

`func (o *TestEndpointParametersBody) HasDate() bool`

HasDate returns a boolean if a field has been set.

### GetDateTime

`func (o *TestEndpointParametersBody) GetDateTime() time.Time`

GetDateTime returns the DateTime field if non-nil, zero value otherwise.

### GetDateTimeOk

`func (o *TestEndpointParametersBody) GetDateTimeOk() (*time.Time, bool)`

GetDateTimeOk returns a tuple with the DateTime field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDateTime

`func (o *TestEndpointParametersBody) SetDateTime(v time.Time)`

SetDateTime sets DateTime field to given value.

### HasDateTime

`func (o *TestEndpointParametersBody) HasDateTime() bool`

HasDateTime returns a boolean if a field has been set.

### GetPassword

`func (o *TestEndpointParametersBody) GetPassword() string`

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *TestEndpointParametersBody) GetPasswordOk() (*string, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword

`func (o *TestEndpointParametersBody) SetPassword(v string)`

SetPassword sets Password field to given value.

### HasPassword

`func (o *TestEndpointParametersBody) HasPassword() bool`

HasPassword returns a boolean if a field has been set.

### GetCallback

`func (o *TestEndpointParametersBody) GetCallback() string`

GetCallback returns the Callback field if non-nil, zero value otherwise.

### GetCallbackOk

`func (o *TestEndpointParametersBody) GetCallbackOk() (*string, bool)`

GetCallbackOk returns a tuple with the Callback field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCallback

`func (o *TestEndpointParametersBody) SetCallback(v string)`

SetCallback sets Callback field to given value.

### HasCallback

`func (o *TestEndpointParametersBody) HasCallback() bool`

HasCallback returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


