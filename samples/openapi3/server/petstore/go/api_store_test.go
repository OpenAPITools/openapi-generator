package main

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	petstoreserver "go-petstore/go"
)

type StoreAPITestService struct {
	petstoreserver.StoreAPIServicer
	placeOrder func(order petstoreserver.Order) (petstoreserver.ImplResponse, error)
}

// PlaceOrder - Place an order for a pet
func (s *StoreAPITestService) PlaceOrder(ctx context.Context, order petstoreserver.Order) (petstoreserver.ImplResponse, error) {
	return s.placeOrder(order)
}

// TestPlaceOrderOK tests PlaceOrder operation. This operation contains Order which has an
// embedding struct that will be tested as well.
func TestPlaceOrderOK(t *testing.T) {
	const (
		payload = `{
			"petId": 6,
			"quantity": 1,
			"comment": "comment",
			"id": 0,
			"shipDate": "2000-01-23T04:56:07.000+00:00",
			"complete": false,
			"status": "placed",
			"RequireTest": "required"
		  }`
		status = http.StatusOK
	)

	bodyReader := strings.NewReader(payload)
	req := httptest.NewRequest(http.MethodPost, "/v2/store/order", bodyReader)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &StoreAPITestService{}
	service.placeOrder = func(order petstoreserver.Order) (petstoreserver.ImplResponse, error) {
		comment := "comment"
		assert.Equal(t, int64(6), order.PetId)
		assert.Equal(t, int32(1), order.Quantity)
		assert.Equal(t, &comment, order.Comment)
		assert.Equal(t, int64(0), order.Id)
		// assert.Equal(t, time.Time, order.ShipDate) // TODO: Fix datetime test
		assert.False(t, order.Complete)
		assert.Equal(t, "placed", order.Status)
		assert.Equal(t, "required", order.RequireTest)

		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewStoreAPIController(service)
	controller, ok := router.(*petstoreserver.StoreAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.PlaceOrder(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Empty(t, data)
}

// TestPlaceOrderFailEmbeddedRequired tests PlaceOrder operation. This operation contains Order which has an
// embedding struct that will be tested as well.
func TestPlaceOrderFailEmbeddedRequired(t *testing.T) {
	const (
		payload = `{
			"petId": 6,
			"quantity": 1,
			"comment": "comment",
			"id": 0,
			"shipDate": "2000-01-23T04:56:07.000+00:00",
			"complete": false,
			"status": "placed"
		  }`
		status = http.StatusUnprocessableEntity
	)

	bodyReader := strings.NewReader(payload)
	req := httptest.NewRequest(http.MethodPost, "/v2/store/order", bodyReader)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &StoreAPITestService{}
	service.placeOrder = func(order petstoreserver.Order) (petstoreserver.ImplResponse, error) {
		assert.FailNow(t, "should not reach this")

		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewStoreAPIController(service)
	controller, ok := router.(*petstoreserver.StoreAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.PlaceOrder(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Equal(t, "\"required field 'requireTest' is zero value.\"\n", string(data))
}
