# FormatTest

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Integer** | Pointer to **int32** |  | [optional] 
**Int32** | Pointer to **int32** |  | [optional] 
**Int64** | Pointer to **int64** |  | [optional] 
**Number** | **float32** |  | 
**Float** | Pointer to **float32** |  | [optional] 
**Double** | Pointer to **float64** |  | [optional] 
**String** | Pointer to **string** |  | [optional] 
**Byte** | **string** |  | 
**Binary** | Pointer to ***os.File** |  | [optional] 
**Date** | **string** |  | 
**DateTime** | Pointer to **time.Time** |  | [optional] 
**Uuid** | Pointer to **string** |  | [optional] 
**Password** | **string** |  | 
**PatternWithDigits** | Pointer to **string** | A string that is a 10 digit number. Can have leading zeros. | [optional] 
**PatternWithDigitsAndDelimiter** | Pointer to **string** | A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01. | [optional] 

## Methods

### NewFormatTest

`func NewFormatTest(number float32, byte_ string, date string, password string, ) *FormatTest`

NewFormatTest instantiates a new FormatTest object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewFormatTestWithDefaults

`func NewFormatTestWithDefaults() *FormatTest`

NewFormatTestWithDefaults instantiates a new FormatTest object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetInteger

`func (o *FormatTest) GetInteger() int32`

GetInteger returns the Integer field if non-nil, zero value otherwise.

### GetIntegerOk

`func (o *FormatTest) GetIntegerOk() (*int32, bool)`

GetIntegerOk returns a tuple with the Integer field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInteger

`func (o *FormatTest) SetInteger(v int32)`

SetInteger sets Integer field to given value.

### HasInteger

`func (o *FormatTest) HasInteger() bool`

HasInteger returns a boolean if a field has been set.

### GetInt32

`func (o *FormatTest) GetInt32() int32`

GetInt32 returns the Int32 field if non-nil, zero value otherwise.

### GetInt32Ok

`func (o *FormatTest) GetInt32Ok() (*int32, bool)`

GetInt32Ok returns a tuple with the Int32 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt32

`func (o *FormatTest) SetInt32(v int32)`

SetInt32 sets Int32 field to given value.

### HasInt32

`func (o *FormatTest) HasInt32() bool`

HasInt32 returns a boolean if a field has been set.

### GetInt64

`func (o *FormatTest) GetInt64() int64`

GetInt64 returns the Int64 field if non-nil, zero value otherwise.

### GetInt64Ok

`func (o *FormatTest) GetInt64Ok() (*int64, bool)`

GetInt64Ok returns a tuple with the Int64 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt64

`func (o *FormatTest) SetInt64(v int64)`

SetInt64 sets Int64 field to given value.

### HasInt64

`func (o *FormatTest) HasInt64() bool`

HasInt64 returns a boolean if a field has been set.

### GetNumber

`func (o *FormatTest) GetNumber() float32`

GetNumber returns the Number field if non-nil, zero value otherwise.

### GetNumberOk

`func (o *FormatTest) GetNumberOk() (*float32, bool)`

GetNumberOk returns a tuple with the Number field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNumber

`func (o *FormatTest) SetNumber(v float32)`

SetNumber sets Number field to given value.


### GetFloat

`func (o *FormatTest) GetFloat() float32`

GetFloat returns the Float field if non-nil, zero value otherwise.

### GetFloatOk

`func (o *FormatTest) GetFloatOk() (*float32, bool)`

GetFloatOk returns a tuple with the Float field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFloat

`func (o *FormatTest) SetFloat(v float32)`

SetFloat sets Float field to given value.

### HasFloat

`func (o *FormatTest) HasFloat() bool`

HasFloat returns a boolean if a field has been set.

### GetDouble

`func (o *FormatTest) GetDouble() float64`

GetDouble returns the Double field if non-nil, zero value otherwise.

### GetDoubleOk

`func (o *FormatTest) GetDoubleOk() (*float64, bool)`

GetDoubleOk returns a tuple with the Double field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDouble

`func (o *FormatTest) SetDouble(v float64)`

SetDouble sets Double field to given value.

### HasDouble

`func (o *FormatTest) HasDouble() bool`

HasDouble returns a boolean if a field has been set.

### GetString

`func (o *FormatTest) GetString() string`

GetString returns the String field if non-nil, zero value otherwise.

### GetStringOk

`func (o *FormatTest) GetStringOk() (*string, bool)`

GetStringOk returns a tuple with the String field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetString

`func (o *FormatTest) SetString(v string)`

SetString sets String field to given value.

### HasString

`func (o *FormatTest) HasString() bool`

HasString returns a boolean if a field has been set.

### GetByte

`func (o *FormatTest) GetByte() string`

GetByte returns the Byte field if non-nil, zero value otherwise.

### GetByteOk

`func (o *FormatTest) GetByteOk() (*string, bool)`

GetByteOk returns a tuple with the Byte field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetByte

`func (o *FormatTest) SetByte(v string)`

SetByte sets Byte field to given value.


### GetBinary

`func (o *FormatTest) GetBinary() *os.File`

GetBinary returns the Binary field if non-nil, zero value otherwise.

### GetBinaryOk

`func (o *FormatTest) GetBinaryOk() (**os.File, bool)`

GetBinaryOk returns a tuple with the Binary field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBinary

`func (o *FormatTest) SetBinary(v *os.File)`

SetBinary sets Binary field to given value.

### HasBinary

`func (o *FormatTest) HasBinary() bool`

HasBinary returns a boolean if a field has been set.

### GetDate

`func (o *FormatTest) GetDate() string`

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *FormatTest) GetDateOk() (*string, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *FormatTest) SetDate(v string)`

SetDate sets Date field to given value.


### GetDateTime

`func (o *FormatTest) GetDateTime() time.Time`

GetDateTime returns the DateTime field if non-nil, zero value otherwise.

### GetDateTimeOk

`func (o *FormatTest) GetDateTimeOk() (*time.Time, bool)`

GetDateTimeOk returns a tuple with the DateTime field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDateTime

`func (o *FormatTest) SetDateTime(v time.Time)`

SetDateTime sets DateTime field to given value.

### HasDateTime

`func (o *FormatTest) HasDateTime() bool`

HasDateTime returns a boolean if a field has been set.

### GetUuid

`func (o *FormatTest) GetUuid() string`

GetUuid returns the Uuid field if non-nil, zero value otherwise.

### GetUuidOk

`func (o *FormatTest) GetUuidOk() (*string, bool)`

GetUuidOk returns a tuple with the Uuid field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUuid

`func (o *FormatTest) SetUuid(v string)`

SetUuid sets Uuid field to given value.

### HasUuid

`func (o *FormatTest) HasUuid() bool`

HasUuid returns a boolean if a field has been set.

### GetPassword

`func (o *FormatTest) GetPassword() string`

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *FormatTest) GetPasswordOk() (*string, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword

`func (o *FormatTest) SetPassword(v string)`

SetPassword sets Password field to given value.


### GetPatternWithDigits

`func (o *FormatTest) GetPatternWithDigits() string`

GetPatternWithDigits returns the PatternWithDigits field if non-nil, zero value otherwise.

### GetPatternWithDigitsOk

`func (o *FormatTest) GetPatternWithDigitsOk() (*string, bool)`

GetPatternWithDigitsOk returns a tuple with the PatternWithDigits field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPatternWithDigits

`func (o *FormatTest) SetPatternWithDigits(v string)`

SetPatternWithDigits sets PatternWithDigits field to given value.

### HasPatternWithDigits

`func (o *FormatTest) HasPatternWithDigits() bool`

HasPatternWithDigits returns a boolean if a field has been set.

### GetPatternWithDigitsAndDelimiter

`func (o *FormatTest) GetPatternWithDigitsAndDelimiter() string`

GetPatternWithDigitsAndDelimiter returns the PatternWithDigitsAndDelimiter field if non-nil, zero value otherwise.

### GetPatternWithDigitsAndDelimiterOk

`func (o *FormatTest) GetPatternWithDigitsAndDelimiterOk() (*string, bool)`

GetPatternWithDigitsAndDelimiterOk returns a tuple with the PatternWithDigitsAndDelimiter field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPatternWithDigitsAndDelimiter

`func (o *FormatTest) SetPatternWithDigitsAndDelimiter(v string)`

SetPatternWithDigitsAndDelimiter sets PatternWithDigitsAndDelimiter field to given value.

### HasPatternWithDigitsAndDelimiter

`func (o *FormatTest) HasPatternWithDigitsAndDelimiter() bool`

HasPatternWithDigitsAndDelimiter returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


