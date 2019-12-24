# Order

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | Pointer to **int64** |  | [optional] 
**PetId** | Pointer to **int64** |  | [optional] 
**Quantity** | Pointer to **int32** |  | [optional] 
**ShipDate** | Pointer to [**time.Time**](time.Time.md) |  | [optional] 
**Status** | Pointer to **string** | Order Status | [optional] 
**Complete** | Pointer to **bool** |  | [optional] [default to false]

## Methods

### GetId

`func (o *Order) GetId() int64`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *Order) GetIdOk() (int64, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasId

`func (o *Order) HasId() bool`

HasId returns a boolean if a field has been set.

### SetId

`func (o *Order) SetId(v int64)`

SetId gets a reference to the given int64 and assigns it to the Id field.

### GetPetId

`func (o *Order) GetPetId() int64`

GetPetId returns the PetId field if non-nil, zero value otherwise.

### GetPetIdOk

`func (o *Order) GetPetIdOk() (int64, bool)`

GetPetIdOk returns a tuple with the PetId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### HasPetId

`func (o *Order) HasPetId() bool`

HasPetId returns a boolean if a field has been set.

### SetPetId

`func (o *Order) SetPetId(v int64)`

SetPetId gets a reference to the given int64 and assigns it to the PetId field.

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


