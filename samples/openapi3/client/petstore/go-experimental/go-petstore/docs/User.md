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
**ArbitraryObject** | Pointer to [**map[string]interface{}**](.md) | test code generation for objects Value must be a map of strings to values. It cannot be the &#39;null&#39; value. | [optional] 
**ArbitraryNullableObject** | Pointer to [**map[string]interface{}**](.md) | test code generation for nullable objects. Value must be a map of strings to values or the &#39;null&#39; value. | [optional] 
**ArbitraryTypeValue** | Pointer to **interface{}** | test code generation for any type Value can be any type - string, number, boolean, array or object. | [optional] 
**ArbitraryNullableTypeValue** | Pointer to **NullableInterface{}** | test code generation for any type Value can be any type - string, number, boolean, array, object or the &#39;null&#39; value. | [optional] 

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

`func (o *User) GetIdOk() (int64, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasId

`func (o *User) HasId() bool`

HasId returns a boolean if a field has been set.

### SetId

`func (o *User) SetId(v int64)`

SetId gets a reference to the given int64 and assigns it to the Id field.

### GetUsername

`func (o *User) GetUsername() string`

GetUsername returns the Username field if non-nil, zero value otherwise.

### GetUsernameOk

`func (o *User) GetUsernameOk() (string, bool)`

GetUsernameOk returns a tuple with the Username field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasUsername

`func (o *User) HasUsername() bool`

HasUsername returns a boolean if a field has been set.

### SetUsername

`func (o *User) SetUsername(v string)`

SetUsername gets a reference to the given string and assigns it to the Username field.

### GetFirstName

`func (o *User) GetFirstName() string`

GetFirstName returns the FirstName field if non-nil, zero value otherwise.

### GetFirstNameOk

`func (o *User) GetFirstNameOk() (string, bool)`

GetFirstNameOk returns a tuple with the FirstName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasFirstName

`func (o *User) HasFirstName() bool`

HasFirstName returns a boolean if a field has been set.

### SetFirstName

`func (o *User) SetFirstName(v string)`

SetFirstName gets a reference to the given string and assigns it to the FirstName field.

### GetLastName

`func (o *User) GetLastName() string`

GetLastName returns the LastName field if non-nil, zero value otherwise.

### GetLastNameOk

`func (o *User) GetLastNameOk() (string, bool)`

GetLastNameOk returns a tuple with the LastName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasLastName

`func (o *User) HasLastName() bool`

HasLastName returns a boolean if a field has been set.

### SetLastName

`func (o *User) SetLastName(v string)`

SetLastName gets a reference to the given string and assigns it to the LastName field.

### GetEmail

`func (o *User) GetEmail() string`

GetEmail returns the Email field if non-nil, zero value otherwise.

### GetEmailOk

`func (o *User) GetEmailOk() (string, bool)`

GetEmailOk returns a tuple with the Email field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasEmail

`func (o *User) HasEmail() bool`

HasEmail returns a boolean if a field has been set.

### SetEmail

`func (o *User) SetEmail(v string)`

SetEmail gets a reference to the given string and assigns it to the Email field.

### GetPassword

`func (o *User) GetPassword() string`

GetPassword returns the Password field if non-nil, zero value otherwise.

### GetPasswordOk

`func (o *User) GetPasswordOk() (string, bool)`

GetPasswordOk returns a tuple with the Password field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPassword

`func (o *User) HasPassword() bool`

HasPassword returns a boolean if a field has been set.

### SetPassword

`func (o *User) SetPassword(v string)`

SetPassword gets a reference to the given string and assigns it to the Password field.

### GetPhone

`func (o *User) GetPhone() string`

GetPhone returns the Phone field if non-nil, zero value otherwise.

### GetPhoneOk

`func (o *User) GetPhoneOk() (string, bool)`

GetPhoneOk returns a tuple with the Phone field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPhone

`func (o *User) HasPhone() bool`

HasPhone returns a boolean if a field has been set.

### SetPhone

`func (o *User) SetPhone(v string)`

SetPhone gets a reference to the given string and assigns it to the Phone field.

### GetUserStatus

`func (o *User) GetUserStatus() int32`

GetUserStatus returns the UserStatus field if non-nil, zero value otherwise.

### GetUserStatusOk

`func (o *User) GetUserStatusOk() (int32, bool)`

GetUserStatusOk returns a tuple with the UserStatus field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasUserStatus

`func (o *User) HasUserStatus() bool`

HasUserStatus returns a boolean if a field has been set.

### SetUserStatus

`func (o *User) SetUserStatus(v int32)`

SetUserStatus gets a reference to the given int32 and assigns it to the UserStatus field.

### GetArbitraryObject

`func (o *User) GetArbitraryObject() map[string]interface{}`

GetArbitraryObject returns the ArbitraryObject field if non-nil, zero value otherwise.

### GetArbitraryObjectOk

`func (o *User) GetArbitraryObjectOk() (map[string]interface{}, bool)`

GetArbitraryObjectOk returns a tuple with the ArbitraryObject field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArbitraryObject

`func (o *User) HasArbitraryObject() bool`

HasArbitraryObject returns a boolean if a field has been set.

### SetArbitraryObject

`func (o *User) SetArbitraryObject(v map[string]interface{})`

SetArbitraryObject gets a reference to the given map[string]interface{} and assigns it to the ArbitraryObject field.

### GetArbitraryNullableObject

`func (o *User) GetArbitraryNullableObject() map[string]interface{}`

GetArbitraryNullableObject returns the ArbitraryNullableObject field if non-nil, zero value otherwise.

### GetArbitraryNullableObjectOk

`func (o *User) GetArbitraryNullableObjectOk() (map[string]interface{}, bool)`

GetArbitraryNullableObjectOk returns a tuple with the ArbitraryNullableObject field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArbitraryNullableObject

`func (o *User) HasArbitraryNullableObject() bool`

HasArbitraryNullableObject returns a boolean if a field has been set.

### SetArbitraryNullableObject

`func (o *User) SetArbitraryNullableObject(v map[string]interface{})`

SetArbitraryNullableObject gets a reference to the given map[string]interface{} and assigns it to the ArbitraryNullableObject field.

### SetArbitraryNullableObjectExplicitNull

`func (o *User) SetArbitraryNullableObjectExplicitNull(b bool)`

SetArbitraryNullableObjectExplicitNull (un)sets ArbitraryNullableObject to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The ArbitraryNullableObject value is set to nil even if false is passed
### GetArbitraryTypeValue

`func (o *User) GetArbitraryTypeValue() interface{}`

GetArbitraryTypeValue returns the ArbitraryTypeValue field if non-nil, zero value otherwise.

### GetArbitraryTypeValueOk

`func (o *User) GetArbitraryTypeValueOk() (interface{}, bool)`

GetArbitraryTypeValueOk returns a tuple with the ArbitraryTypeValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArbitraryTypeValue

`func (o *User) HasArbitraryTypeValue() bool`

HasArbitraryTypeValue returns a boolean if a field has been set.

### SetArbitraryTypeValue

`func (o *User) SetArbitraryTypeValue(v interface{})`

SetArbitraryTypeValue gets a reference to the given interface{} and assigns it to the ArbitraryTypeValue field.

### GetArbitraryNullableTypeValue

`func (o *User) GetArbitraryNullableTypeValue() NullableInterface{}`

GetArbitraryNullableTypeValue returns the ArbitraryNullableTypeValue field if non-nil, zero value otherwise.

### GetArbitraryNullableTypeValueOk

`func (o *User) GetArbitraryNullableTypeValueOk() (NullableInterface{}, bool)`

GetArbitraryNullableTypeValueOk returns a tuple with the ArbitraryNullableTypeValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasArbitraryNullableTypeValue

`func (o *User) HasArbitraryNullableTypeValue() bool`

HasArbitraryNullableTypeValue returns a boolean if a field has been set.

### SetArbitraryNullableTypeValue

`func (o *User) SetArbitraryNullableTypeValue(v NullableInterface{})`

SetArbitraryNullableTypeValue gets a reference to the given NullableInterface{} and assigns it to the ArbitraryNullableTypeValue field.

### SetArbitraryNullableTypeValueExplicitNull

`func (o *User) SetArbitraryNullableTypeValueExplicitNull(b bool)`

SetArbitraryNullableTypeValueExplicitNull (un)sets ArbitraryNullableTypeValue to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The ArbitraryNullableTypeValue value is set to nil even if false is passed

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


