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

type UserAPITestService struct {
	petstoreserver.UserAPIServicer
	createUser func(user petstoreserver.User) (petstoreserver.ImplResponse, error)
	loginUser  func(username string, password string, int32Test int32, int64Test int64, float32Test float32,
		float64Test float64, booleanTest bool) (petstoreserver.ImplResponse, error)
}

// CreateUser - Create user
func (s *UserAPITestService) CreateUser(ctx context.Context, user petstoreserver.User) (petstoreserver.ImplResponse, error) {
	return s.createUser(user)
}

// LoginUser - Logs user into the system
func (s *UserAPITestService) LoginUser(ctx context.Context, username string, password string, int32Test int32,
	int64Test int64, float32Test float32, float64Test float64, booleanTest bool) (petstoreserver.ImplResponse, error) {
	return s.loginUser(username, password, int32Test, int64Test, float32Test, float64Test, booleanTest)
}

func TestCreateUserOK(t *testing.T) {
	const (
		payload = `{
			"firstName": "firstName",
			"lastName": "lastName",
			"password": "password",
			"userStatus": 6,
			"phone": "phone",
			"deepSliceModel": [
			  [
				[
				  {
					"name": "name",
					"id": 1
				  }
				]
			  ]
			],
			"id": 0,
			"deepSliceMap": [
			  [
				{
				  "tag": {
					"name": "name",
					"id": 1
				  },
				  "Pet": [
					{
					  "photoUrls": [
						"photoUrls",
						"photoUrls"
					  ],
					  "name": "doggie",
					  "id": 0,
					  "category": {
						"name": "name",
						"id": 6
					  },
					  "tags": [
						{
						  "name": "name",
						  "id": 1
						}
					  ],
					  "status": "available"
					}
				  ]
				}
			  ]
			],
			"email": "email",
			"username": "username"
		  }`
		status = http.StatusOK
	)

	bodyReader := strings.NewReader(payload)
	req := httptest.NewRequest(http.MethodPost, "/v2/user", bodyReader)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &UserAPITestService{}
	service.createUser = func(user petstoreserver.User) (petstoreserver.ImplResponse, error) {
		phone := "phone"
		assert.Equal(t, "firstName", user.FirstName)
		assert.Equal(t, "lastName", user.LastName)
		assert.Equal(t, "password", user.Password)
		assert.Equal(t, int32(6), user.UserStatus)
		assert.Equal(t, &phone, user.Phone)
		assert.Equal(t, &[][][]petstoreserver.Tag{
			{
				{
					{Id: 1, Name: "name"},
				},
			},
		}, user.DeepSliceModel)
		assert.Equal(t, int64(0), user.Id)
		assert.Equal(t, "email", user.Email)
		assert.Equal(t, "username", user.Username)

		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewUserAPIController(service)
	controller, ok := router.(*petstoreserver.UserAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.CreateUser(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Empty(t, data)
}

func TestCreateUserOKNullablePhone(t *testing.T) {
	const (
		payload = `{
			"firstName": "firstName",
			"lastName": "lastName",
			"password": "password",
			"userStatus": 6,
			"deepSliceModel": [
			  [
				[
				  {
					"name": "name",
					"id": 1
				  }
				]
			  ]
			],
			"id": 0,
			"deepSliceMap": [
			  [
				{
				  "tag": {
					"name": "name",
					"id": 1
				  },
				  "Pet": [
					{
					  "photoUrls": [
						"photoUrls",
						"photoUrls"
					  ],
					  "name": "doggie",
					  "id": 0,
					  "category": {
						"name": "name",
						"id": 6
					  },
					  "tags": [
						{
						  "name": "name",
						  "id": 1
						}
					  ],
					  "status": "available"
					}
				  ]
				}
			  ]
			],
			"email": "email",
			"username": "username"
		  }`
		status = http.StatusOK
	)

	bodyReader := strings.NewReader(payload)
	req := httptest.NewRequest(http.MethodPost, "/v2/user", bodyReader)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &UserAPITestService{}
	service.createUser = func(user petstoreserver.User) (petstoreserver.ImplResponse, error) {
		assert.Equal(t, "firstName", user.FirstName)
		assert.Equal(t, "lastName", user.LastName)
		assert.Equal(t, "password", user.Password)
		assert.Equal(t, int32(6), user.UserStatus)
		assert.Nil(t, user.Phone)
		assert.Equal(t, &[][][]petstoreserver.Tag{
			{
				{
					{Id: 1, Name: "name"},
				},
			},
		}, user.DeepSliceModel)
		assert.Equal(t, int64(0), user.Id)
		assert.Equal(t, "email", user.Email)
		assert.Equal(t, "username", user.Username)

		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewUserAPIController(service)
	controller, ok := router.(*petstoreserver.UserAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.CreateUser(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Empty(t, data)
}

func TestCreateUserFailDeepSliceModelRequired(t *testing.T) {
	const (
		payload = `{
			"firstName": "firstName",
			"lastName": "lastName",
			"password": "password",
			"userStatus": 6,
			"phone": "phone",
			"id": 0,
			"email": "email",
			"username": "username"
		  }`
		status = http.StatusUnprocessableEntity
	)

	bodyReader := strings.NewReader(payload)
	req := httptest.NewRequest(http.MethodPost, "/v2/user", bodyReader)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &UserAPITestService{}
	service.createUser = func(user petstoreserver.User) (petstoreserver.ImplResponse, error) {
		assert.FailNow(t, "should not reach this")
		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewUserAPIController(service)
	controller, ok := router.(*petstoreserver.UserAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.CreateUser(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Equal(t, "\"required field 'deepSliceModel' is zero value.\"\n", string(data))
}

// func TestCreateUserFailDeepSliceModelEmptyRecursive(t *testing.T) {
// 	const (
// 		payload = `{
// 			"firstName": "firstName",
// 			"lastName": "lastName",
// 			"password": "password",
// 			"userStatus": 6,
// 			"phone": "phone",
// 			"deepSliceModel": [
// 				[
// 				]
// 			  ],
// 			"id": 0,
// 			"email": "email",
// 			"username": "username"
// 		  }`
// 		status = http.StatusUnprocessableEntity
// 	)

// 	bodyReader := strings.NewReader(payload)
// 	req := httptest.NewRequest(http.MethodPost, "/v2/user", bodyReader)
// 	w := httptest.NewRecorder()

// 	// Create the service and inject the logic
// 	service := &UserAPITestService{}
// 	service.createUser = func(user petstoreserver.User) (petstoreserver.ImplResponse, error) {
// 		assert.FailNow(t, "should not reach this")
// 		return petstoreserver.Response(status, nil), nil
// 	}

// 	// Create the controller with the service
// 	router := petstoreserver.NewUserAPIController(service)
// 	controller, ok := router.(*petstoreserver.UserAPIController)
// 	require.True(t, ok)

// 	// Call the method of controller we are testing
// 	controller.CreateUser(w, req)

// 	res := w.Result()
// 	assert.Equal(t, status, res.StatusCode)
// 	defer res.Body.Close()

// 	// Check the response
// 	data, err := io.ReadAll(res.Body)
// 	require.NoError(t, err)
// 	assert.Equal(t, "\"required field 'deepSliceModel' is zero value.\"\n", string(data))
// }

func TestLoginUserOK(t *testing.T) {
	const (
		status = http.StatusOK
	)

	req := httptest.NewRequest(http.MethodPost, "/user/login?username=test&int32_test=1&int64_test=2&float32_test=1.1&float64_test=1.2&boolean_test=true", nil)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &UserAPITestService{}
	service.loginUser = func(username, password string, int32Test int32, int64Test int64, float32Test float32, float64Test float64, booleanTest bool) (petstoreserver.ImplResponse, error) {
		assert.Equal(t, "test", username)
		assert.Equal(t, int32(1), int32Test)
		assert.Equal(t, int64(2), int64Test)
		assert.Equal(t, float32(1.1), float32Test)
		assert.Equal(t, float64(1.2), float64Test)
		assert.True(t, booleanTest)

		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewUserAPIController(service)
	controller, ok := router.(*petstoreserver.UserAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.LoginUser(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Empty(t, data)
}

func TestLoginUserOKOptional(t *testing.T) {
	const (
		status = http.StatusOK
	)

	req := httptest.NewRequest(http.MethodPost, "/user/login?username=test", nil)
	w := httptest.NewRecorder()

	// Create the service and inject the logic
	service := &UserAPITestService{}
	service.loginUser = func(username, password string, int32Test int32, int64Test int64, float32Test float32,
		float64Test float64, booleanTest bool) (petstoreserver.ImplResponse, error) {
		assert.Equal(t, "test", username)
		assert.Zero(t, int32Test)
		assert.Zero(t, int64Test)
		assert.Zero(t, float32Test)
		assert.Zero(t, float64Test)
		assert.Zero(t, booleanTest)

		return petstoreserver.Response(status, nil), nil
	}

	// Create the controller with the service
	router := petstoreserver.NewUserAPIController(service)
	controller, ok := router.(*petstoreserver.UserAPIController)
	require.True(t, ok)

	// Call the method of controller we are testing
	controller.LoginUser(w, req)

	res := w.Result()
	assert.Equal(t, status, res.StatusCode)
	defer res.Body.Close()

	// Check the response
	data, err := io.ReadAll(res.Body)
	require.NoError(t, err)
	assert.Empty(t, data)
}
