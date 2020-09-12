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
**Binary** | Pointer to [***os.File**](*os.File.md) | None | [optional] 
**Date** | Pointer to **string** | None | [optional] 
**DateTime** | Pointer to [**time.Time**](time.Time.md) | None | [optional] 
**Password** | Pointer to **string** | None | [optional] 
**Callback** | Pointer to **string** | None | [optional] 

## Methods

### NewTestEndpointParametersBody

`func NewTestEndpointParametersBody(Number float32, Double float64, PatternWithoutDelimiter string, Byte string, ) *TestEndpointParametersBody`

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

`func (o *TestEndpointParametersBody) GetInteger() `

GetInteger returns the Integer field if non-nil, zero value otherwise.

### GetIntegerOk

`func (o *TestEndpointParametersBody) GetIntegerOk() (*, bool)`

GetIntegerOk returns a tuple with the Integer field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInteger

`func (o *TestEndpointParametersBody) SetInteger(v )`

SetInteger sets Integer field to given value.

### HasInteger

`func (o *TestEndpointParametersBody) HasInteger() bool`

HasInteger returns a boolean if a field has been set.

### GetInt32

`func (o *TestEndpointParametersBody) GetInt32() `

GetInt32 returns the Int32 field if non-nil, zero value otherwise.

### GetInt32Ok

`func (o *TestEndpointParametersBody) GetInt32Ok() (*, bool)`

GetInt32Ok returns a tuple with the Int32 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt32

`func (o *TestEndpointParametersBody) SetInt32(v )`

SetInt32 sets Int32 field to given value.

### HasInt32

`func (o *TestEndpointParametersBody) HasInt32() bool`

HasInt32 returns a boolean if a field has been set.

### GetInt64

`func (o *TestEndpointParametersBody) GetInt64() `

GetInt64 returns the Int64 field if non-nil, zero value otherwise.

### GetInt64Ok

`func (o *TestEndpointParametersBody) GetInt64Ok() (*, bool)`

GetInt64Ok returns a tuple with the Int64 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetInt64

`func (o *TestEndpointParametersBody) SetInt64(v )`

SetInt64 sets Int64 field to given value.

### HasInt64

`func (o *TestEndpointParametersBody) HasInt64() bool`

HasInt64 returns a boolean if a field has been set.

### GetNumber

`func (o *TestEndpointParametersBody) GetNumber() `

GetNumber returns the Number field if non-nil, zero value otherwise.

### GetNumberOk

`func (o *TestEndpointParametersBody) GetNumberOk() (*, bool)`

GetNumberOk returns a tuple with the Number field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNumber

`func (o *TestEndpointParametersBody) SetNumber(v )`

SetNumber sets Number field to given value.


### GetFloat

`func (o *TestEndpointParametersBody) GetFloat() `

GetFloat returns the Float field if non-nil, zero value otherwise.

### GetFloatOk

`func (o *TestEndpointParametersBody) GetFloatOk() (*, bool)`

GetFloatOk returns a tuple with the Float field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFloat

`func (o *TestEndpointParametersBody) SetFloat(v )`

SetFloat sets Float field to given value.

### HasFloat

`func (o *TestEndpointParametersBody) HasFloat() bool`

HasFloat returns a boolean if a field has been set.

### GetDouble

`func (o *TestEndpointParametersBody) GetDouble() `

GetDouble returns the Double field if non-nil, zero value otherwise.

### GetDoubleOk

`func (o *TestEndpointParametersBody) GetDoubleOk() (*, bool)`

GetDoubleOk returns a tuple with the Double field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDouble

`func (o *TestEndpointParametersBody) SetDouble(v )`

SetDouble sets Double field to given value.


### GetString

`func (o *TestEndpointParametersBody) GetString() `

GetString returns the String field if non-nil, zero value otherwise.

### GetStringOk

`func (o *TestEndpointParametersBody) GetStringOk() (*, bool)`

GetStringOk returns a tuple with the String field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetString

`func (o *TestEndpointParametersBody) SetString(v )`

SetString sets String field to given value.

### HasString

`func (o *TestEndpointParametersBody) HasString() bool`

HasString returns a boolean if a field has been set.

### GetPatternWithoutDelimiter

`func (o *TestEndpointParametersBody) GetPatternWithoutDelimiter() `

GetPatternWithoutDelimiter returns the PatternWithoutDelimiter field if non-nil, zero value otherwise.

### GetPatternWithoutDelimiterOk

`func (o *TestEndpointParametersBody) GetPatternWithoutDelimiterOk() (*, bool)`

GetPatternWithoutDelimiterOk returns a tuple with the PatternWithoutDelimiter field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPatternWithoutDelimiter

`func (o *TestEndpointParametersBody) SetPatternWithoutDelimiter(v )`

SetPatternWithoutDelimiter sets PatternWithoutDelimiter field to given value.


### GetByte

`func (o *TestEndpointParametersBody) GetByte() `

GetByte returns the Byte field if non-nil, zero value otherwise.

### GetByteOk

`func (o *TestEndpointParametersBody) GetByteOk() (*, bool)`

GetByteOk returns a tuple with the Byte field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetByte

`func (o *TestEndpointParametersBody) SetByte(v )`

SetByte sets Byte field to given value.


### GetBinary

`func (o *TestEndpointParametersBody) GetBinary() `

GetBinary returns the Binary field if non-nil, zero value otherwise.

### GetBinaryOk

`func (o *TestEndpointParametersBody) GetBinaryOk() (*, bool)`

GetBinaryOk returns a tuple with the Binary field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetBinary

`func (o *TestEndpointParametersBody) SetBinary(v )`

SetBinary sets Binary field to given value.

### HasBinary

`func (o *TestEndpointParametersBody) HasBinary() bool`

HasBinary returns a boolean if a field has been set.

### GetDate

`func (o *TestEndpointParametersBody) GetDate() `

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *TestEndpointParametersBody) GetDateOk() (*, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *TestEndpointParametersBody) SetDate(v )`

SetDate sets Date field to given value.

### HasDate

`func (o *TestEndpointParametersBody) HasDate() bool`

HasDate returns a boolean if a field has been set.

### GetDateTime

`func (o *TestEndpointParametersBody) GetDateTime() `

GetDateTime returns the DateTime field if non-nil, zero value otherwise.

### GetDateTimeOk

`func (o *TestEndpointParametersBody) GetDateTimeOk() (*, bool)`

GetDateTimeOk returns a tuple with the DateTime field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDateTime

`func (o *TestEndpointParametersBody) SetDateTime(v )`

SetDateTime sets DateTime field to given value.

### HasDateTime

`func (o *TestEndpointParametersBody) HasDateTime() bool`

HasDateTime returns a boolean if a field has been set.

### GetPassword

`func (o *TestEndpointParametersBody) GetPassword() `

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *TestEndpointParametersBody) GetPasswordOk() (*, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword

`func (o *TestEndpointParametersBody) SetPassword(v )`

SetPassword sets Password field to given value.

### HasPassword

`func (o *TestEndpointParametersBody) HasPassword() bool`

HasPassword returns a boolean if a field has been set.

### GetCallback

`func (o *TestEndpointParametersBody) GetCallback() `

GetCallback returns the Callback field if non-nil, zero value otherwise.

### GetCallbackOk

`func (o *TestEndpointParametersBody) GetCallbackOk() (*, bool)`

GetCallbackOk returns a tuple with the Callback field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCallback

`func (o *TestEndpointParametersBody) SetCallback(v )`

SetCallback sets Callback field to given value.

### HasCallback

`func (o *TestEndpointParametersBody) HasCallback() bool`

HasCallback returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


