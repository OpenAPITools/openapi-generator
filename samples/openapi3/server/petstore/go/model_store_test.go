package main

import (
	"testing"

	petstoreserver "go-petstore/go"

	"github.com/stretchr/testify/assert"
)

// TestPlaceOrderOK tests PlaceOrder operation. This operation contains Order which has an
// embedding struct that will be tested as well.
func TestDefaultValues(t *testing.T) {
	m := petstoreserver.NewOrderWithDefaults()

	assert.Equal(t, "placed", m.Status)

	// Order object contain this definition
	// allOf:
	// - $ref: '#/components/schemas/OrderInfo'
	// - $ref: '#/components/schemas/SpecialInfo'

	// Quantity comes from composition with OrderInfo
	assert.Equal(t, int32(1), m.Quantity)

	// SpecialInfo is embedded in Order struct
	assert.Equal(t, "dog", m.SpecialInfo.Type)
}
