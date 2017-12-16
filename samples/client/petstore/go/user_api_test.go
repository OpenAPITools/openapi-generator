package main

import (
	"testing"

	sw "./go-petstore"
	"golang.org/x/net/context"

	"github.com/stretchr/testify/assert"
)

func TestCreateUser(t *testing.T) {
	newUser := sw.User{
		Id:         1000,
		FirstName:  "gopher",
		LastName:   "lang",
		Username:   "gopher",
		Password:   "lang",
		Email:      "lang@test.com",
		Phone:      "5101112222",
		UserStatus: 1}

	apiResponse, err := client.UserApi.CreateUser(context.Background(), newUser)

	if err != nil {
		t.Errorf("Error while adding user")
		t.Log(err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}

//adding x to skip the test, currently it is failing
func TestCreateUsersWithArrayInput(t *testing.T) {
	newUsers := []sw.User{
		sw.User{
			Id:         int64(1001),
			FirstName:  "gopher1",
			LastName:   "lang1",
			Username:   "gopher1",
			Password:   "lang1",
			Email:      "lang1@test.com",
			Phone:      "5101112222",
			UserStatus: int32(1),
		},
		sw.User{
			Id:         int64(1002),
			FirstName:  "gopher2",
			LastName:   "lang2",
			Username:   "gopher2",
			Password:   "lang2",
			Email:      "lang2@test.com",
			Phone:      "5101112222",
			UserStatus: int32(1),
		},
	}

	apiResponse, err := client.UserApi.CreateUsersWithArrayInput(context.Background(), newUsers)
	if err != nil {
		t.Errorf("Error while adding users")
		t.Log(err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}

	//tear down
	_, err1 := client.UserApi.DeleteUser(context.Background(), "gopher1")
	if err1 != nil {
		t.Errorf("Error while deleting user")
		t.Log(err1)
	}

	_, err2 := client.UserApi.DeleteUser(context.Background(), "gopher2")
	if err2 != nil {
		t.Errorf("Error while deleting user")
		t.Log(err2)
	}
}

func TestGetUserByName(t *testing.T) {
	assert := assert.New(t)

	resp, apiResponse, err := client.UserApi.GetUserByName(context.Background(), "gopher")
	if err != nil {
		t.Errorf("Error while getting user by id")
		t.Log(err)
	} else {
		assert.Equal(resp.Id, int64(1000), "User id should be equal")
		assert.Equal(resp.Username, "gopher", "User name should be gopher")
		assert.Equal(resp.LastName, "lang", "Last name should be lang")
		//t.Log(resp)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}

func TestGetUserByNameWithInvalidID(t *testing.T) {
	resp, apiResponse, err := client.UserApi.GetUserByName(context.Background(), "999999999")
	if apiResponse != nil && apiResponse.StatusCode == 404 {
		return // This is a pass condition. API will return with a 404 error.
	} else if err != nil {
		t.Errorf("Error while getting user by invalid id")
		t.Log(err)
		t.Log(apiResponse)
	} else {
		t.Log(resp)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}

func TestUpdateUser(t *testing.T) {
	assert := assert.New(t)

	newUser := sw.User{
		Id:         1000,
		FirstName:  "gopher20",
		LastName:   "lang20",
		Username:   "gopher",
		Password:   "lang",
		Email:      "lang@test.com",
		Phone:      "5101112222",
		UserStatus: 1}

	apiResponse, err := client.UserApi.UpdateUser(context.Background(), "gopher", newUser)
	if err != nil {
		t.Errorf("Error while deleting user by id")
		t.Log(err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}

	//verify changings are correct
	resp, apiResponse, err := client.UserApi.GetUserByName(context.Background(), "gopher")
	if err != nil {
		t.Errorf("Error while getting user by id")
		t.Log(err)
	} else {
		assert.Equal(resp.Id, int64(1000), "User id should be equal")
		assert.Equal(resp.FirstName, "gopher20", "User name should be gopher")
		assert.Equal(resp.Password, "lang", "User name should be the same")
	}
}

func TestDeleteUser(t *testing.T) {
	apiResponse, err := client.UserApi.DeleteUser(context.Background(), "gopher")

	if err != nil {
		t.Errorf("Error while deleting user")
		t.Log(err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}
