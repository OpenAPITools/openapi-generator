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
**ObjectWithNoDeclaredProps** | Pointer to [**ObjectType**](.md) | test code generation for objects Value must be a map of strings to values. It cannot be the &#39;null&#39; value. | [optional] 
**ObjectWithNoDeclaredPropsNullable** | Pointer to [**ObjectType**](.md) | test code generation for nullable objects. Value must be a map of strings to values or the &#39;null&#39; value. | [optional] 
**AnyTypeProp** | Pointer to [**AnyType**](.md) | test code generation for any type Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389 | [optional] 
**AnyTypeExceptNullProp** | Pointer to **ObjectType** | any type except &#39;null&#39; Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. | [optional] 
**AnyTypePropNullable** | Pointer to [**AnyType**](.md) | test code generation for any type Here the &#39;type&#39; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The &#39;nullable&#39; attribute does not change the allowed values. | [optional] 

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

### GetObjectWithNoDeclaredProps

`func (o *User) GetObjectWithNoDeclaredProps() ObjectType`

GetObjectWithNoDeclaredProps returns the ObjectWithNoDeclaredProps field if non-nil, zero value otherwise.

### GetObjectWithNoDeclaredPropsOk

`func (o *User) GetObjectWithNoDeclaredPropsOk() (*ObjectType, bool)`

GetObjectWithNoDeclaredPropsOk returns a tuple with the ObjectWithNoDeclaredProps field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetObjectWithNoDeclaredProps

`func (o *User) SetObjectWithNoDeclaredProps(v ObjectType)`

SetObjectWithNoDeclaredProps sets ObjectWithNoDeclaredProps field to given value.

### HasObjectWithNoDeclaredProps

`func (o *User) HasObjectWithNoDeclaredProps() bool`

HasObjectWithNoDeclaredProps returns a boolean if a field has been set.

### GetObjectWithNoDeclaredPropsNullable

`func (o *User) GetObjectWithNoDeclaredPropsNullable() ObjectType`

GetObjectWithNoDeclaredPropsNullable returns the ObjectWithNoDeclaredPropsNullable field if non-nil, zero value otherwise.

### GetObjectWithNoDeclaredPropsNullableOk

`func (o *User) GetObjectWithNoDeclaredPropsNullableOk() (*ObjectType, bool)`

GetObjectWithNoDeclaredPropsNullableOk returns a tuple with the ObjectWithNoDeclaredPropsNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetObjectWithNoDeclaredPropsNullable

`func (o *User) SetObjectWithNoDeclaredPropsNullable(v ObjectType)`

SetObjectWithNoDeclaredPropsNullable sets ObjectWithNoDeclaredPropsNullable field to given value.

### HasObjectWithNoDeclaredPropsNullable

`func (o *User) HasObjectWithNoDeclaredPropsNullable() bool`

HasObjectWithNoDeclaredPropsNullable returns a boolean if a field has been set.

### SetObjectWithNoDeclaredPropsNullableNil

`func (o *User) SetObjectWithNoDeclaredPropsNullableNil(b bool)`

 SetObjectWithNoDeclaredPropsNullableNil sets the value for ObjectWithNoDeclaredPropsNullable to be an explicit nil

### UnsetObjectWithNoDeclaredPropsNullable
`func (o *User) UnsetObjectWithNoDeclaredPropsNullable()`

UnsetObjectWithNoDeclaredPropsNullable ensures that no value is present for ObjectWithNoDeclaredPropsNullable, not even an explicit nil
### GetAnyTypeProp

`func (o *User) GetAnyTypeProp() AnyType`

GetAnyTypeProp returns the AnyTypeProp field if non-nil, zero value otherwise.

### GetAnyTypePropOk

`func (o *User) GetAnyTypePropOk() (*AnyType, bool)`

GetAnyTypePropOk returns a tuple with the AnyTypeProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAnyTypeProp

`func (o *User) SetAnyTypeProp(v AnyType)`

SetAnyTypeProp sets AnyTypeProp field to given value.

### HasAnyTypeProp

`func (o *User) HasAnyTypeProp() bool`

HasAnyTypeProp returns a boolean if a field has been set.

### SetAnyTypePropNil

`func (o *User) SetAnyTypePropNil(b bool)`

 SetAnyTypePropNil sets the value for AnyTypeProp to be an explicit nil

### UnsetAnyTypeProp
`func (o *User) UnsetAnyTypeProp()`

UnsetAnyTypeProp ensures that no value is present for AnyTypeProp, not even an explicit nil
### GetAnyTypeExceptNullProp

`func (o *User) GetAnyTypeExceptNullProp() ObjectType`

GetAnyTypeExceptNullProp returns the AnyTypeExceptNullProp field if non-nil, zero value otherwise.

### GetAnyTypeExceptNullPropOk

`func (o *User) GetAnyTypeExceptNullPropOk() (*ObjectType, bool)`

GetAnyTypeExceptNullPropOk returns a tuple with the AnyTypeExceptNullProp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAnyTypeExceptNullProp

`func (o *User) SetAnyTypeExceptNullProp(v ObjectType)`

SetAnyTypeExceptNullProp sets AnyTypeExceptNullProp field to given value.

### HasAnyTypeExceptNullProp

`func (o *User) HasAnyTypeExceptNullProp() bool`

HasAnyTypeExceptNullProp returns a boolean if a field has been set.

### GetAnyTypePropNullable

`func (o *User) GetAnyTypePropNullable() AnyType`

GetAnyTypePropNullable returns the AnyTypePropNullable field if non-nil, zero value otherwise.

### GetAnyTypePropNullableOk

`func (o *User) GetAnyTypePropNullableOk() (*AnyType, bool)`

GetAnyTypePropNullableOk returns a tuple with the AnyTypePropNullable field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAnyTypePropNullable

`func (o *User) SetAnyTypePropNullable(v AnyType)`

SetAnyTypePropNullable sets AnyTypePropNullable field to given value.

### HasAnyTypePropNullable

`func (o *User) HasAnyTypePropNullable() bool`

HasAnyTypePropNullable returns a boolean if a field has been set.

### SetAnyTypePropNullableNil

`func (o *User) SetAnyTypePropNullableNil(b bool)`

 SetAnyTypePropNullableNil sets the value for AnyTypePropNullable to be an explicit nil

### UnsetAnyTypePropNullable
`func (o *User) UnsetAnyTypePropNullable()`

UnsetAnyTypePropNullable ensures that no value is present for AnyTypePropNullable, not even an explicit nil

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


