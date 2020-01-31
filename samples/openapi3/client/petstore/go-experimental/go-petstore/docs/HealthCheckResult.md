# HealthCheckResult

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**NullableMessage** | Pointer to **NullableString** |  | [optional] 

## Methods

### NewHealthCheckResult

`func NewHealthCheckResult() *HealthCheckResult`

NewHealthCheckResult instantiates a new HealthCheckResult object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewHealthCheckResultWithDefaults

`func NewHealthCheckResultWithDefaults() *HealthCheckResult`

NewHealthCheckResultWithDefaults instantiates a new HealthCheckResult object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetNullableMessage

`func (o *HealthCheckResult) GetNullableMessage() NullableString`

GetNullableMessage returns the NullableMessage field if non-nil, zero value otherwise.

### GetNullableMessageOk

`func (o *HealthCheckResult) GetNullableMessageOk() (NullableString, bool)`

GetNullableMessageOk returns a tuple with the NullableMessage field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasNullableMessage

`func (o *HealthCheckResult) HasNullableMessage() bool`

HasNullableMessage returns a boolean if a field has been set.

### SetNullableMessage

`func (o *HealthCheckResult) SetNullableMessage(v NullableString)`

SetNullableMessage gets a reference to the given NullableString and assigns it to the NullableMessage field.

### SetNullableMessageExplicitNull

`func (o *HealthCheckResult) SetNullableMessageExplicitNull(b bool)`

SetNullableMessageExplicitNull (un)sets NullableMessage to be considered as explicit "null" value
when serializing to JSON (pass true as argument to set this, false to unset)
The NullableMessage value is set to nil even if false is passed

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


