package main

import (
	"context"
	"regexp"
	"testing"
	"time"

	sw "./go-petstore"
)

func TestPlaceOrder(t *testing.T) {
	newOrder := sw.Order{
		Id:       sw.Int64(0),
		PetId:    sw.Int64(0),
		Quantity: sw.Int32(0),
		ShipDate: sw.Time(time.Now().UTC()),
		Status:   sw.String("placed"),
		Complete: sw.Bool(false)}

	_, r, err := client.StoreApi.PlaceOrder(context.Background(), newOrder)

	if err != nil {
		// Skip parsing time error due to error in Petstore Test Server
		// https://github.com/OpenAPITools/openapi-generator/issues/1292
		if regexp.
			MustCompile(`^parsing time.+cannot parse "\+0000"" as "Z07:00"$`).
			MatchString(err.Error()) {
			t.Log("Skipping error for parsing time with `+0000` UTC offset as Petstore Test Server does not return valid RFC 3339 datetime")
		} else {
			t.Fatalf("Error while placing order: %v", err)
		}
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
