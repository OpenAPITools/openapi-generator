# BigCat

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
<<<<<<< HEAD
**ClassName** | Pointer to **string** |  | 
**Color** | Pointer to **string** |  | [optional] [default to red]
**Declawed** | Pointer to **bool** |  | [optional] 
**Kind** | Pointer to [**BigCatAllOfKind**](BigCatAllOfKind.md) |  | [optional] 
=======
**Kind** | Pointer to **string** |  | [optional] 
>>>>>>> origin/master

## Methods

### NewBigCat

`func NewBigCat() *BigCat`

NewBigCat instantiates a new BigCat object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewBigCatWithDefaults

`func NewBigCatWithDefaults() *BigCat`

NewBigCatWithDefaults instantiates a new BigCat object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetKind

`func (o *BigCat) GetKind() BigCatAllOfKind`

GetKind returns the Kind field if non-nil, zero value otherwise.

### GetKindOk

<<<<<<< HEAD
`func (o *BigCat) GetKindOk() (BigCatAllOfKind, bool)`
=======
`func (o *BigCat) GetKindOk() (*string, bool)`
>>>>>>> origin/master

GetKindOk returns a tuple with the Kind field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetKind

`func (o *BigCat) SetKind(v string)`

SetKind sets Kind field to given value.

### HasKind

<<<<<<< HEAD
`func (o *BigCat) SetKind(v BigCatAllOfKind)`

SetKind gets a reference to the given BigCatAllOfKind and assigns it to the Kind field.
=======
`func (o *BigCat) HasKind() bool`

HasKind returns a boolean if a field has been set.
>>>>>>> origin/master


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


