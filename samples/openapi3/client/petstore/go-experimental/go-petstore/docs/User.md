# User

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | Pointer to **int64** |  | [optional] 
**Username** | Pointer to **string** |  | [optional] 
**FirstName** | Pointer to **string** |  | [optional] 
**LastName** | Pointer to **string** |  | [optional] 
**Email** | Pointer to **string** |  | [optional] 
**Password** | Pointer to **string** |  | [optional] 
**Phone** | Pointer to **string** |  | [optional] 
**UserStatus** | Pointer to **int32** | User Status | [optional] 
**ArbitraryObject** | Pointer to **map[string]interface{}** | test code generation for objects Value must be a map of strings to values. It cannot be the &#39;null&#39; value. | [optional] 
**ArbitraryNullableObject** | Pointer to **map[string]interface{}** | test code generation for nullable objects. Value must be a map of strings to values or the &#39;null&#39; value. | [optional] 
**ArbitraryTypeValue** | Pointer to **interface{}** | test code generation for any type Value can be any type - string, number, boolean, array or object. | [optional] 
**ArbitraryNullableTypeValue** | Pointer to **interface{}** | test code generation for any type Value can be any type - string, number, boolean, array, object or the &#39;null&#39; value. | [optional] 

## Methods

### NewUser

`func NewUser() *User`

NewUser instantiates a new User object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUserWithDefaults

`func NewUserWithDefaults() *User`

NewUserWithDefaults instantiates a new User object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *User) GetId() int64`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *User) GetIdOk() (*int64, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *User) SetId(v int64)`

SetId sets Id field to given value.

### HasId

`func (o *User) HasId() bool`

HasId returns a boolean if a field has been set.

### GetUsername

`func (o *User) GetUsername() string`

GetUsername returns the Username field if non-nil, zero value otherwise.

### GetUsernameOk

`func (o *User) GetUsernameOk() (*string, bool)`

GetUsernameOk returns a tuple with the Username field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUsername

`func (o *User) SetUsername(v string)`

SetUsername sets Username field to given value.

### HasUsername

`func (o *User) HasUsername() bool`

HasUsername returns a boolean if a field has been set.

### GetFirstName

`func (o *User) GetFirstName() string`

GetFirstName returns the FirstName field if non-nil, zero value otherwise.

### GetFirstNameOk

`func (o *User) GetFirstNameOk() (*string, bool)`

GetFirstNameOk returns a tuple with the FirstName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFirstName

`func (o *User) SetFirstName(v string)`

SetFirstName sets FirstName field to given value.

### HasFirstName

`func (o *User) HasFirstName() bool`

HasFirstName returns a boolean if a field has been set.

### GetLastName

`func (o *User) GetLastName() string`

GetLastName returns the LastName field if non-nil, zero value otherwise.

### GetLastNameOk

`func (o *User) GetLastNameOk() (*string, bool)`

GetLastNameOk returns a tuple with the LastName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastName

`func (o *User) SetLastName(v string)`

SetLastName sets LastName field to given value.

### HasLastName

`func (o *User) HasLastName() bool`

HasLastName returns a boolean if a field has been set.

### GetEmail

`func (o *User) GetEmail() string`

GetEmail returns the Email field if non-nil, zero value otherwise.

### GetEmailOk

`func (o *User) GetEmailOk() (*string, bool)`

GetEmailOk returns a tuple with the Email field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEmail

`func (o *User) SetEmail(v string)`

SetEmail sets Email field to given value.

### HasEmail

`func (o *User) HasEmail() bool`

HasEmail returns a boolean if a field has been set.

### GetPassword

`func (o *User) GetPassword() string`

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *User) GetPasswordOk() (*string, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword

`func (o *User) SetPassword(v string)`

SetPassword sets Password field to given value.

### HasPassword

`func (o *User) HasPassword() bool`

HasPassword returns a boolean if a field has been set.

### GetPhone

`func (o *User) GetPhone() string`

GetPhone returns the Phone field if non-nil, zero value otherwise.

### GetPhoneOk

`func (o *User) GetPhoneOk() (*string, bool)`

GetPhoneOk returns a tuple with the Phone field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPhone

`func (o *User) SetPhone(v string)`

SetPhone sets Phone field to given value.

### HasPhone

`func (o *User) HasPhone() bool`

HasPhone returns a boolean if a field has been set.

### GetUserStatus

`func (o *User) GetUserStatus() int32`

GetUserStatus returns the UserStatus field if non-nil, zero value otherwise.

### GetUserStatusOk

`func (o *User) GetUserStatusOk() (*int32, bool)`

GetUserStatusOk returns a tuple with the UserStatus field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUserStatus

`func (o *User) SetUserStatus(v int32)`

SetUserStatus sets UserStatus field to given value.

### HasUserStatus

`func (o *User) HasUserStatus() bool`

HasUserStatus returns a boolean if a field has been set.

### GetArbitraryObject

`func (o *User) GetArbitraryObject() map[string]interface{}`

GetArbitraryObject returns the ArbitraryObject field if non-nil, zero value otherwise.

### GetArbitraryObjectOk

`func (o *User) GetArbitraryObjectOk() (*map[string]interface{}, bool)`

GetArbitraryObjectOk returns a tuple with the ArbitraryObject field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArbitraryObject

`func (o *User) SetArbitraryObject(v map[string]interface{})`

SetArbitraryObject sets ArbitraryObject field to given value.

### HasArbitraryObject

`func (o *User) HasArbitraryObject() bool`

HasArbitraryObject returns a boolean if a field has been set.

### GetArbitraryNullableObject

`func (o *User) GetArbitraryNullableObject() map[string]interface{}`

GetArbitraryNullableObject returns the ArbitraryNullableObject field if non-nil, zero value otherwise.

### GetArbitraryNullableObjectOk

`func (o *User) GetArbitraryNullableObjectOk() (*map[string]interface{}, bool)`

GetArbitraryNullableObjectOk returns a tuple with the ArbitraryNullableObject field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArbitraryNullableObject

`func (o *User) SetArbitraryNullableObject(v map[string]interface{})`

SetArbitraryNullableObject sets ArbitraryNullableObject field to given value.

### HasArbitraryNullableObject

`func (o *User) HasArbitraryNullableObject() bool`

HasArbitraryNullableObject returns a boolean if a field has been set.

### SetArbitraryNullableObjectNil

`func (o *User) SetArbitraryNullableObjectNil(b bool)`

 SetArbitraryNullableObjectNil sets the value for ArbitraryNullableObject to be an explicit nil

### UnsetArbitraryNullableObject
`func (o *User) UnsetArbitraryNullableObject()`

UnsetArbitraryNullableObject ensures that no value is present for ArbitraryNullableObject, not even an explicit nil
### GetArbitraryTypeValue

`func (o *User) GetArbitraryTypeValue() interface{}`

GetArbitraryTypeValue returns the ArbitraryTypeValue field if non-nil, zero value otherwise.

### GetArbitraryTypeValueOk

`func (o *User) GetArbitraryTypeValueOk() (*interface{}, bool)`

GetArbitraryTypeValueOk returns a tuple with the ArbitraryTypeValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArbitraryTypeValue

`func (o *User) SetArbitraryTypeValue(v interface{})`

SetArbitraryTypeValue sets ArbitraryTypeValue field to given value.

### HasArbitraryTypeValue

`func (o *User) HasArbitraryTypeValue() bool`

HasArbitraryTypeValue returns a boolean if a field has been set.

### SetArbitraryTypeValueNil

`func (o *User) SetArbitraryTypeValueNil(b bool)`

 SetArbitraryTypeValueNil sets the value for ArbitraryTypeValue to be an explicit nil

### UnsetArbitraryTypeValue
`func (o *User) UnsetArbitraryTypeValue()`

UnsetArbitraryTypeValue ensures that no value is present for ArbitraryTypeValue, not even an explicit nil
### GetArbitraryNullableTypeValue

`func (o *User) GetArbitraryNullableTypeValue() interface{}`

GetArbitraryNullableTypeValue returns the ArbitraryNullableTypeValue field if non-nil, zero value otherwise.

### GetArbitraryNullableTypeValueOk

`func (o *User) GetArbitraryNullableTypeValueOk() (*interface{}, bool)`

GetArbitraryNullableTypeValueOk returns a tuple with the ArbitraryNullableTypeValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetArbitraryNullableTypeValue

`func (o *User) SetArbitraryNullableTypeValue(v interface{})`

SetArbitraryNullableTypeValue sets ArbitraryNullableTypeValue field to given value.

### HasArbitraryNullableTypeValue

`func (o *User) HasArbitraryNullableTypeValue() bool`

HasArbitraryNullableTypeValue returns a boolean if a field has been set.

### SetArbitraryNullableTypeValueNil

`func (o *User) SetArbitraryNullableTypeValueNil(b bool)`

 SetArbitraryNullableTypeValueNil sets the value for ArbitraryNullableTypeValue to be an explicit nil

### UnsetArbitraryNullableTypeValue
`func (o *User) UnsetArbitraryNullableTypeValue()`

UnsetArbitraryNullableTypeValue ensures that no value is present for ArbitraryNullableTypeValue, not even an explicit nil

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


