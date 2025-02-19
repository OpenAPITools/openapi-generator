# FilterAny

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Type** | **string** |  | 
**Regex** | Pointer to **string** |  | [optional] 
**Data** | Pointer to **[]string** |  | [optional] 

## Methods

### NewFilterAny

`func NewFilterAny(type_ string, ) *FilterAny`

NewFilterAny instantiates a new FilterAny object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewFilterAnyWithDefaults

`func NewFilterAnyWithDefaults() *FilterAny`

NewFilterAnyWithDefaults instantiates a new FilterAny object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetType

`func (o *FilterAny) GetType() string`

GetType returns the Type field if non-nil, zero value otherwise.

### GetTypeOk

`func (o *FilterAny) GetTypeOk() (*string, bool)`

GetTypeOk returns a tuple with the Type field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetType

`func (o *FilterAny) SetType(v string)`

SetType sets Type field to given value.


### GetRegex

`func (o *FilterAny) GetRegex() string`

GetRegex returns the Regex field if non-nil, zero value otherwise.

### GetRegexOk

`func (o *FilterAny) GetRegexOk() (*string, bool)`

GetRegexOk returns a tuple with the Regex field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetRegex

`func (o *FilterAny) SetRegex(v string)`

SetRegex sets Regex field to given value.

### HasRegex

`func (o *FilterAny) HasRegex() bool`

HasRegex returns a boolean if a field has been set.

### GetData

`func (o *FilterAny) GetData() []string`

GetData returns the Data field if non-nil, zero value otherwise.

### GetDataOk

`func (o *FilterAny) GetDataOk() (*[]string, bool)`

GetDataOk returns a tuple with the Data field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetData

`func (o *FilterAny) SetData(v []string)`

SetData sets Data field to given value.

### HasData

`func (o *FilterAny) HasData() bool`

HasData returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


