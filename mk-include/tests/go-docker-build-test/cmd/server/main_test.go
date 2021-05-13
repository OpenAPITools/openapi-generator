package main

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestHealthCheckHandler(t *testing.T) {
	// 1. create request obj
	req, err := http.NewRequest("GET", HealthCheckEndpoint, nil)
	require.NoError(t, err)

	// 2. create a recorder
	recorder := httptest.NewRecorder()

	// 3. process request
	mux := newMux()
	mux.ServeHTTP(recorder, req)

	// Check the status code is what we expect.
	require.Equal(t, recorder.Code, http.StatusOK)
	require.Equal(t, recorder.Body.String(), "OK")
}

func TestHelloHandler(t *testing.T) {
	// 1. create request obj
	req, err := http.NewRequest("GET", HelloEndpoint, nil)
	require.NoError(t, err)

	// 2. create a recorder
	recorder := httptest.NewRecorder()

	// 3. process request
	mux := newMux()
	mux.ServeHTTP(recorder, req)

	// Check the status code is what we expect.
	require.Equal(t, recorder.Code, http.StatusOK)
	require.Equal(t, recorder.Body.String(), "Hello World")
}
