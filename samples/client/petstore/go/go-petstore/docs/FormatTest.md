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
**BigDecimal** | Pointer to **float64** |  | [optional] 

## Methods

### NewFormatTest

`func NewFormatTest(Number float32, Byte string, Date string, Password string, ) *FormatTest`

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

`func (o *FormatTest) GetInteger() `

GetInteger returns the Integer field if non-nil, zero value otherwise.

### GetIntegerOk

`func (o *FormatTest) GetIntegerOk() (*, bool)`

GetIntegerOk returns a tuple with the Integer field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInteger

`func (o *FormatTest) SetInteger(v )`

SetInteger sets Integer field to given value.

### HasInteger

`func (o *FormatTest) HasInteger() bool`

HasInteger returns a boolean if a field has been set.

### GetInt32

`func (o *FormatTest) GetInt32() `

GetInt32 returns the Int32 field if non-nil, zero value otherwise.

### GetInt32Ok

`func (o *FormatTest) GetInt32Ok() (*, bool)`

GetInt32Ok returns a tuple with the Int32 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt32

`func (o *FormatTest) SetInt32(v )`

SetInt32 sets Int32 field to given value.

### HasInt32

`func (o *FormatTest) HasInt32() bool`

HasInt32 returns a boolean if a field has been set.

### GetInt64

`func (o *FormatTest) GetInt64() `

GetInt64 returns the Int64 field if non-nil, zero value otherwise.

### GetInt64Ok

`func (o *FormatTest) GetInt64Ok() (*, bool)`

GetInt64Ok returns a tuple with the Int64 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt64

`func (o *FormatTest) SetInt64(v )`

SetInt64 sets Int64 field to given value.

### HasInt64

`func (o *FormatTest) HasInt64() bool`

HasInt64 returns a boolean if a field has been set.

### GetNumber

`func (o *FormatTest) GetNumber() `

GetNumber returns the Number field if non-nil, zero value otherwise.

### GetNumberOk

`func (o *FormatTest) GetNumberOk() (*, bool)`

GetNumberOk returns a tuple with the Number field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNumber

`func (o *FormatTest) SetNumber(v )`

SetNumber sets Number field to given value.


### GetFloat

`func (o *FormatTest) GetFloat() `

GetFloat returns the Float field if non-nil, zero value otherwise.

### GetFloatOk

`func (o *FormatTest) GetFloatOk() (*, bool)`

GetFloatOk returns a tuple with the Float field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFloat

`func (o *FormatTest) SetFloat(v )`

SetFloat sets Float field to given value.

### HasFloat

`func (o *FormatTest) HasFloat() bool`

HasFloat returns a boolean if a field has been set.

### GetDouble

`func (o *FormatTest) GetDouble() `

GetDouble returns the Double field if non-nil, zero value otherwise.

### GetDoubleOk

`func (o *FormatTest) GetDoubleOk() (*, bool)`

GetDoubleOk returns a tuple with the Double field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDouble

`func (o *FormatTest) SetDouble(v )`

SetDouble sets Double field to given value.

### HasDouble

`func (o *FormatTest) HasDouble() bool`

HasDouble returns a boolean if a field has been set.

### GetString

`func (o *FormatTest) GetString() `

GetString returns the String field if non-nil, zero value otherwise.

### GetStringOk

`func (o *FormatTest) GetStringOk() (*, bool)`

GetStringOk returns a tuple with the String field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetString

`func (o *FormatTest) SetString(v )`

SetString sets String field to given value.

### HasString

`func (o *FormatTest) HasString() bool`

HasString returns a boolean if a field has been set.

### GetByte

`func (o *FormatTest) GetByte() `

GetByte returns the Byte field if non-nil, zero value otherwise.

### GetByteOk

`func (o *FormatTest) GetByteOk() (*, bool)`

GetByteOk returns a tuple with the Byte field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetByte

`func (o *FormatTest) SetByte(v )`

SetByte sets Byte field to given value.


### GetBinary

`func (o *FormatTest) GetBinary() `

GetBinary returns the Binary field if non-nil, zero value otherwise.

### GetBinaryOk

`func (o *FormatTest) GetBinaryOk() (*, bool)`

GetBinaryOk returns a tuple with the Binary field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBinary

`func (o *FormatTest) SetBinary(v )`

SetBinary sets Binary field to given value.

### HasBinary

`func (o *FormatTest) HasBinary() bool`

HasBinary returns a boolean if a field has been set.

### GetDate

`func (o *FormatTest) GetDate() `

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *FormatTest) GetDateOk() (*, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *FormatTest) SetDate(v )`

SetDate sets Date field to given value.


### GetDateTime

`func (o *FormatTest) GetDateTime() `

GetDateTime returns the DateTime field if non-nil, zero value otherwise.

### GetDateTimeOk

`func (o *FormatTest) GetDateTimeOk() (*, bool)`

GetDateTimeOk returns a tuple with the DateTime field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDateTime

`func (o *FormatTest) SetDateTime(v )`

SetDateTime sets DateTime field to given value.

### HasDateTime

`func (o *FormatTest) HasDateTime() bool`

HasDateTime returns a boolean if a field has been set.

### GetUuid

`func (o *FormatTest) GetUuid() `

GetUuid returns the Uuid field if non-nil, zero value otherwise.

### GetUuidOk

`func (o *FormatTest) GetUuidOk() (*, bool)`

GetUuidOk returns a tuple with the Uuid field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUuid

`func (o *FormatTest) SetUuid(v )`

SetUuid sets Uuid field to given value.

### HasUuid

`func (o *FormatTest) HasUuid() bool`

HasUuid returns a boolean if a field has been set.

### GetPassword

`func (o *FormatTest) GetPassword() `

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *FormatTest) GetPasswordOk() (*, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword

`func (o *FormatTest) SetPassword(v )`

SetPassword sets Password field to given value.


### GetBigDecimal

`func (o *FormatTest) GetBigDecimal() `

GetBigDecimal returns the BigDecimal field if non-nil, zero value otherwise.

### GetBigDecimalOk

`func (o *FormatTest) GetBigDecimalOk() (*, bool)`

GetBigDecimalOk returns a tuple with the BigDecimal field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBigDecimal

`func (o *FormatTest) SetBigDecimal(v )`

SetBigDecimal sets BigDecimal field to given value.

### HasBigDecimal

`func (o *FormatTest) HasBigDecimal() bool`

HasBigDecimal returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


