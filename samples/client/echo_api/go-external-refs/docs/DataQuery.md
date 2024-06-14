# DataQuery

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Suffix** | Pointer to **string** | test suffix | [optional] 
**Text** | Pointer to **string** | Some text containing white spaces | [optional] 
**Date** | Pointer to **time.Time** | A date | [optional] 

## Methods

### NewDataQuery

`func NewDataQuery() *DataQuery`

NewDataQuery instantiates a new DataQuery object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewDataQueryWithDefaults

`func NewDataQueryWithDefaults() *DataQuery`

NewDataQueryWithDefaults instantiates a new DataQuery object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetSuffix

`func (o *DataQuery) GetSuffix() string`

GetSuffix returns the Suffix field if non-nil, zero value otherwise.

### GetSuffixOk

`func (o *DataQuery) GetSuffixOk() (*string, bool)`

GetSuffixOk returns a tuple with the Suffix field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSuffix

`func (o *DataQuery) SetSuffix(v string)`

SetSuffix sets Suffix field to given value.

### HasSuffix

`func (o *DataQuery) HasSuffix() bool`

HasSuffix returns a boolean if a field has been set.

### GetText

`func (o *DataQuery) GetText() string`

GetText returns the Text field if non-nil, zero value otherwise.

### GetTextOk

`func (o *DataQuery) GetTextOk() (*string, bool)`

GetTextOk returns a tuple with the Text field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetText

`func (o *DataQuery) SetText(v string)`

SetText sets Text field to given value.

### HasText

`func (o *DataQuery) HasText() bool`

HasText returns a boolean if a field has been set.

### GetDate

`func (o *DataQuery) GetDate() time.Time`

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *DataQuery) GetDateOk() (*time.Time, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *DataQuery) SetDate(v time.Time)`

SetDate sets Date field to given value.

### HasDate

`func (o *DataQuery) HasDate() bool`

HasDate returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


