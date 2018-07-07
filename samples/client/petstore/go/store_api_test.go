package main

import (
	"context"
	"testing"
	"time"

	sw "./go-petstore"
)

func TestPlaceOrder(t *testing.T) {
	newOrder := sw.Order{
		Id:       0,
		PetId:    0,
		Quantity: 0,
		ShipDate: time.Now().UTC(),
		Status:   "placed",
		Complete: false}

	_, r, err := client.StoreApi.PlaceOrder(context.Background(), newOrder)

	if err != nil {
		t.Errorf("Error while placing order")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
