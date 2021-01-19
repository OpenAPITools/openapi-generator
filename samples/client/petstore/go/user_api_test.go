package main

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	sw "./go-petstore"
)

func TestCreateUser(t *testing.T) {
	newUser := sw.User{
		Id:         sw.PtrInt64(1000),
		FirstName:  sw.PtrString("gopher"),
		LastName:   sw.PtrString("lang"),
		Username:   sw.PtrString("gopher"),
		Password:   sw.PtrString("lang"),
		Email:      sw.PtrString("lang@test.com"),
		Phone:      sw.PtrString("5101112222"),
		UserStatus: sw.PtrInt32(1)}

	apiResponse, err := client.UserApi.CreateUser(context.Background()).Body(newUser).Execute()

	if err != nil {
		t.Fatalf("Error while adding user: %v", err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}

//adding x to skip the test, currently it is failing
func TestCreateUsersWithArrayInput(t *testing.T) {
	newUsers := []sw.User{
		sw.User{
			Id:         sw.PtrInt64(1001),
			FirstName:  sw.PtrString("gopher1"),
			LastName:   sw.PtrString("lang1"),
			Username:   sw.PtrString("gopher1"),
			Password:   sw.PtrString("lang1"),
			Email:      sw.PtrString("lang1@test.com"),
			Phone:      sw.PtrString("5101112222"),
			UserStatus: sw.PtrInt32(1),
		},
		sw.User{
			Id:         sw.PtrInt64(1002),
			FirstName:  sw.PtrString("gopher2"),
			LastName:   sw.PtrString("lang2"),
			Username:   sw.PtrString("gopher2"),
			Password:   sw.PtrString("lang2"),
			Email:      sw.PtrString("lang2@test.com"),
			Phone:      sw.PtrString("5101112222"),
			UserStatus: sw.PtrInt32(1),
		},
	}

	apiResponse, err := client.UserApi.CreateUsersWithArrayInput(context.Background()).Body(newUsers).Execute()
	if err != nil {
		t.Fatalf("Error while adding users: %v", err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
/* issue deleting users due to issue in the server side (500). commented out below for the time being
	//tear down
	_, err1 := client.UserApi.DeleteUser(context.Background(), "gopher1").Execute()
	if err1 != nil {
		t.Errorf("Error while deleting user")
		t.Log(err1)
	}

	_, err2 := client.UserApi.DeleteUser(context.Background(), "gopher2").Execute()
	if err2 != nil {
		t.Errorf("Error while deleting user")
		t.Log(err2)
	}
*/
}

func TestGetUserByName(t *testing.T) {
	assert := assert.New(t)

	resp, apiResponse, err := client.UserApi.GetUserByName(context.Background(), "gopher").Execute()
	if err != nil {
		t.Fatalf("Error while getting user by id: %v", err)
	} else {
		assert.Equal(*resp.Id, int64(1000), "User id should be equal")
		assert.Equal(*resp.Username, "gopher", "User name should be gopher")
		assert.Equal(*resp.LastName, "lang", "Last name should be lang")
		//t.Log(resp)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}

func TestGetUserByNameWithInvalidID(t *testing.T) {
	resp, apiResponse, err := client.UserApi.GetUserByName(context.Background(), "999999999").Execute()
	if apiResponse != nil && apiResponse.StatusCode == 404 {
		return // This is a pass condition. API will return with a 404 error.
	} else if err != nil {
		t.Fatalf("Error while getting user by invalid id: %v", err)
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
		Id:         sw.PtrInt64(1000),
		FirstName:  sw.PtrString("gopher20"),
		LastName:   sw.PtrString("lang20"),
		Username:   sw.PtrString("gopher"),
		Password:   sw.PtrString("lang"),
		Email:      sw.PtrString("lang@test.com"),
		Phone:      sw.PtrString("5101112222"),
		UserStatus: sw.PtrInt32(1)}

	apiResponse, err := client.UserApi.UpdateUser(context.Background(), "gopher").Body(newUser).Execute()
	if err != nil {
		t.Fatalf("Error while deleting user by id: %v", err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}

	//verify changings are correct
	resp, apiResponse, err := client.UserApi.GetUserByName(context.Background(), "gopher").Execute()
	if err != nil {
		t.Fatalf("Error while getting user by id: %v", err)
	} else {
		assert.Equal(*resp.Id, int64(1000), "User id should be equal")
		assert.Equal(*resp.FirstName, "gopher20", "User name should be gopher")
		assert.Equal(*resp.Password, "lang", "User name should be the same")
	}
}

/* issue deleting users due to issue in the server side (500). commented out below for the time being
func TestDeleteUser(t *testing.T) {
	apiResponse, err := client.UserApi.DeleteUser(context.Background(), "gopher").Execute()

	if err != nil {
		t.Fatalf("Error while deleting user: %v", err)
	}
	if apiResponse.StatusCode != 200 {
		t.Log(apiResponse)
	}
}
*/
