# Order

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ID** | Pointer to **int64** |  | [optional] 
**PetID** | Pointer to **int64** |  | [optional] 
**Quantity** | Pointer to **int32** |  | [optional] 
**ShipDate** | Pointer to [**time.Time**](time.Time.md) |  | [optional] 
**Status** | Pointer to **string** | Order Status | [optional] 
**Complete** | Pointer to **bool** |  | [optional] [default to false]

## Methods

### GetID

`func (o *Order) GetID() int64`

GetID returns the ID field if non-nil, zero value otherwise.

### GetIDOk

`func (o *Order) GetIDOk() (int64, bool)`

GetIDOk returns a tuple with the ID field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasID

`func (o *Order) HasID() bool`

HasID returns a boolean if a field has been set.

### SetID

`func (o *Order) SetID(v int64)`

SetID gets a reference to the given int64 and assigns it to the ID field.

### GetPetID

`func (o *Order) GetPetID() int64`

GetPetID returns the PetID field if non-nil, zero value otherwise.

### GetPetIDOk

`func (o *Order) GetPetIDOk() (int64, bool)`

GetPetIDOk returns a tuple with the PetID field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPetID

`func (o *Order) HasPetID() bool`

HasPetID returns a boolean if a field has been set.

### SetPetID

`func (o *Order) SetPetID(v int64)`

SetPetID gets a reference to the given int64 and assigns it to the PetID field.

### GetQuantity

`func (o *Order) GetQuantity() int32`

GetQuantity returns the Quantity field if non-nil, zero value otherwise.

### GetQuantityOk

`func (o *Order) GetQuantityOk() (int32, bool)`

GetQuantityOk returns a tuple with the Quantity field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasQuantity

`func (o *Order) HasQuantity() bool`

HasQuantity returns a boolean if a field has been set.

### SetQuantity

`func (o *Order) SetQuantity(v int32)`

SetQuantity gets a reference to the given int32 and assigns it to the Quantity field.

### GetShipDate

`func (o *Order) GetShipDate() time.Time`

GetShipDate returns the ShipDate field if non-nil, zero value otherwise.

### GetShipDateOk

`func (o *Order) GetShipDateOk() (time.Time, bool)`

GetShipDateOk returns a tuple with the ShipDate field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasShipDate

`func (o *Order) HasShipDate() bool`

HasShipDate returns a boolean if a field has been set.

### SetShipDate

`func (o *Order) SetShipDate(v time.Time)`

SetShipDate gets a reference to the given time.Time and assigns it to the ShipDate field.

### GetStatus

`func (o *Order) GetStatus() string`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *Order) GetStatusOk() (string, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasStatus

`func (o *Order) HasStatus() bool`

HasStatus returns a boolean if a field has been set.

### SetStatus

`func (o *Order) SetStatus(v string)`

SetStatus gets a reference to the given string and assigns it to the Status field.

### GetComplete

`func (o *Order) GetComplete() bool`

GetComplete returns the Complete field if non-nil, zero value otherwise.

### GetCompleteOk

`func (o *Order) GetCompleteOk() (bool, bool)`

GetCompleteOk returns a tuple with the Complete field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasComplete

`func (o *Order) HasComplete() bool`

HasComplete returns a boolean if a field has been set.

### SetComplete

`func (o *Order) SetComplete(v bool)`

SetComplete gets a reference to the given bool and assigns it to the Complete field.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


