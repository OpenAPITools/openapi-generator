package petstoreserver

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type DefaultAPITestService struct {
	DefaultAPIServicer
	update func(testObject TestObject) (ImplResponse, error)
}

func (s *DefaultAPITestService) Update(ctx context.Context, testObject TestObject) (ImplResponse, error) {
	return s.update(testObject)
}

func TestUpdateAcceptsRequiredZeroValue(t *testing.T) {
	const (
		payload = `{"name":"","n":0}`
		status  = http.StatusNoContent
	)

	req := httptest.NewRequest(http.MethodPut, "/config", strings.NewReader(payload))
	w := httptest.NewRecorder()

	service := &DefaultAPITestService{}
	service.update = func(testObject TestObject) (ImplResponse, error) {
		assert.Equal(t, "", testObject.Name)
		assert.Equal(t, int32(0), testObject.N)
		return Response(status, nil), nil
	}

	controller := NewDefaultAPIController(service)
	controller.Update(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Empty(t, data)
}

func TestUpdateRejectsMissingRequiredField(t *testing.T) {
	const (
		payload = `{"name":""}`
		status  = http.StatusUnprocessableEntity
	)

	req := httptest.NewRequest(http.MethodPut, "/config", strings.NewReader(payload))
	w := httptest.NewRecorder()

	service := &DefaultAPITestService{}
	service.update = func(testObject TestObject) (ImplResponse, error) {
		assert.FailNow(t, "service should not be called when required field is missing")
		return Response(status, nil), nil
	}

	controller := NewDefaultAPIController(service)
	controller.Update(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Equal(t, "\"field 'n' is required.\"\n", string(data))
}

func TestUpdateRejectsUnknownField(t *testing.T) {
	const (
		payload = `{"name":"","n":0,"unknown":true}`
		status  = http.StatusBadRequest
	)

	req := httptest.NewRequest(http.MethodPut, "/config", strings.NewReader(payload))
	w := httptest.NewRecorder()

	service := &DefaultAPITestService{}
	service.update = func(testObject TestObject) (ImplResponse, error) {
		assert.FailNow(t, "service should not be called when an unknown field is present")
		return Response(status, nil), nil
	}

	controller := NewDefaultAPIController(service)
	controller.Update(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Contains(t, string(data), "unknown field")
}

func TestTestObjectUnmarshalJSONZeroValue(t *testing.T) {
	var obj TestObject
	err := json.Unmarshal([]byte(`{"name":"","n":0}`), &obj)
	require.NoError(t, err)
	assert.Equal(t, "", obj.Name)
	assert.Equal(t, int32(0), obj.N)
}
