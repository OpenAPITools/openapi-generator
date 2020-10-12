# InlineObject3

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
**Binary** | Pointer to [***os.File**](*os.File.md) | None | [optional] 
**Date** | Pointer to **string** | None | [optional] 
**DateTime** | Pointer to [**time.Time**](time.Time.md) | None | [optional] 
**Password** | Pointer to **string** | None | [optional] 
**Callback** | Pointer to **string** | None | [optional] 

## Methods

### NewInlineObject3

`func NewInlineObject3(Number float32, Double float64, PatternWithoutDelimiter string, Byte string, ) *InlineObject3`

NewInlineObject3 instantiates a new InlineObject3 object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewInlineObject3WithDefaults

`func NewInlineObject3WithDefaults() *InlineObject3`

NewInlineObject3WithDefaults instantiates a new InlineObject3 object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetInteger

`func (o *InlineObject3) GetInteger() `

GetInteger returns the Integer field if non-nil, zero value otherwise.

### GetIntegerOk

`func (o *InlineObject3) GetIntegerOk() (*, bool)`

GetIntegerOk returns a tuple with the Integer field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInteger

`func (o *InlineObject3) SetInteger(v )`

SetInteger sets Integer field to given value.

### HasInteger

`func (o *InlineObject3) HasInteger() bool`

HasInteger returns a boolean if a field has been set.

### GetInt32

`func (o *InlineObject3) GetInt32() `

GetInt32 returns the Int32 field if non-nil, zero value otherwise.

### GetInt32Ok

`func (o *InlineObject3) GetInt32Ok() (*, bool)`

GetInt32Ok returns a tuple with the Int32 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt32

`func (o *InlineObject3) SetInt32(v )`

SetInt32 sets Int32 field to given value.

### HasInt32

`func (o *InlineObject3) HasInt32() bool`

HasInt32 returns a boolean if a field has been set.

### GetInt64

`func (o *InlineObject3) GetInt64() `

GetInt64 returns the Int64 field if non-nil, zero value otherwise.

### GetInt64Ok

`func (o *InlineObject3) GetInt64Ok() (*, bool)`

GetInt64Ok returns a tuple with the Int64 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt64

`func (o *InlineObject3) SetInt64(v )`

SetInt64 sets Int64 field to given value.

### HasInt64

`func (o *InlineObject3) HasInt64() bool`

HasInt64 returns a boolean if a field has been set.

### GetNumber

`func (o *InlineObject3) GetNumber() `

GetNumber returns the Number field if non-nil, zero value otherwise.

### GetNumberOk

`func (o *InlineObject3) GetNumberOk() (*, bool)`

GetNumberOk returns a tuple with the Number field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNumber

`func (o *InlineObject3) SetNumber(v )`

SetNumber sets Number field to given value.


### GetFloat

`func (o *InlineObject3) GetFloat() `

GetFloat returns the Float field if non-nil, zero value otherwise.

### GetFloatOk

`func (o *InlineObject3) GetFloatOk() (*, bool)`

GetFloatOk returns a tuple with the Float field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFloat

`func (o *InlineObject3) SetFloat(v )`

SetFloat sets Float field to given value.

### HasFloat

`func (o *InlineObject3) HasFloat() bool`

HasFloat returns a boolean if a field has been set.

### GetDouble

`func (o *InlineObject3) GetDouble() `

GetDouble returns the Double field if non-nil, zero value otherwise.

### GetDoubleOk

`func (o *InlineObject3) GetDoubleOk() (*, bool)`

GetDoubleOk returns a tuple with the Double field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDouble

`func (o *InlineObject3) SetDouble(v )`

SetDouble sets Double field to given value.


### GetString

`func (o *InlineObject3) GetString() `

GetString returns the String field if non-nil, zero value otherwise.

### GetStringOk

`func (o *InlineObject3) GetStringOk() (*, bool)`

GetStringOk returns a tuple with the String field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetString

`func (o *InlineObject3) SetString(v )`

SetString sets String field to given value.

### HasString

`func (o *InlineObject3) HasString() bool`

HasString returns a boolean if a field has been set.

### GetPatternWithoutDelimiter

`func (o *InlineObject3) GetPatternWithoutDelimiter() `

GetPatternWithoutDelimiter returns the PatternWithoutDelimiter field if non-nil, zero value otherwise.

### GetPatternWithoutDelimiterOk

`func (o *InlineObject3) GetPatternWithoutDelimiterOk() (*, bool)`

GetPatternWithoutDelimiterOk returns a tuple with the PatternWithoutDelimiter field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPatternWithoutDelimiter

`func (o *InlineObject3) SetPatternWithoutDelimiter(v )`

SetPatternWithoutDelimiter sets PatternWithoutDelimiter field to given value.


### GetByte

`func (o *InlineObject3) GetByte() `

GetByte returns the Byte field if non-nil, zero value otherwise.

### GetByteOk

`func (o *InlineObject3) GetByteOk() (*, bool)`

GetByteOk returns a tuple with the Byte field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetByte

`func (o *InlineObject3) SetByte(v )`

SetByte sets Byte field to given value.


### GetBinary

`func (o *InlineObject3) GetBinary() `

GetBinary returns the Binary field if non-nil, zero value otherwise.

### GetBinaryOk

`func (o *InlineObject3) GetBinaryOk() (*, bool)`

GetBinaryOk returns a tuple with the Binary field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBinary

`func (o *InlineObject3) SetBinary(v )`

SetBinary sets Binary field to given value.

### HasBinary

`func (o *InlineObject3) HasBinary() bool`

HasBinary returns a boolean if a field has been set.

### GetDate

`func (o *InlineObject3) GetDate() `

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *InlineObject3) GetDateOk() (*, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *InlineObject3) SetDate(v )`

SetDate sets Date field to given value.

### HasDate

`func (o *InlineObject3) HasDate() bool`

HasDate returns a boolean if a field has been set.

### GetDateTime

`func (o *InlineObject3) GetDateTime() `

GetDateTime returns the DateTime field if non-nil, zero value otherwise.

### GetDateTimeOk

`func (o *InlineObject3) GetDateTimeOk() (*, bool)`

GetDateTimeOk returns a tuple with the DateTime field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDateTime

`func (o *InlineObject3) SetDateTime(v )`

SetDateTime sets DateTime field to given value.

### HasDateTime

`func (o *InlineObject3) HasDateTime() bool`

HasDateTime returns a boolean if a field has been set.

### GetPassword

`func (o *InlineObject3) GetPassword() `

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *InlineObject3) GetPasswordOk() (*, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword

`func (o *InlineObject3) SetPassword(v )`

SetPassword sets Password field to given value.

### HasPassword

`func (o *InlineObject3) HasPassword() bool`

HasPassword returns a boolean if a field has been set.

### GetCallback

`func (o *InlineObject3) GetCallback() `

GetCallback returns the Callback field if non-nil, zero value otherwise.

### GetCallbackOk

`func (o *InlineObject3) GetCallbackOk() (*, bool)`

GetCallbackOk returns a tuple with the Callback field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCallback

`func (o *InlineObject3) SetCallback(v )`

SetCallback sets Callback field to given value.

### HasCallback

`func (o *InlineObject3) HasCallback() bool`

HasCallback returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


