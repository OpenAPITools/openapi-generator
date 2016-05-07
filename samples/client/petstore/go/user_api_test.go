package main

import (
  sw "./go-petstore"
  "github.com/stretchr/testify/assert"
  "testing"
)

func TestCreateUser(t *testing.T) {
  s := sw.NewUserApi()
  newUser := sw.User{
    Id: 1000, 
    FirstName: "gopher",
    LastName : "lang",
    Username : "gopher",
    Password : "lang",
    Email : "lang@test.com",
    Phone : "5101112222",
    UserStatus: 1}

  apiResponse, err := s.CreateUser(newUser)

  if err != nil {
    t.Errorf("Error while adding user")
    t.Log(err)
  }
  if apiResponse.Response.StatusCode != 200 {
    t.Log(apiResponse.Response)
  } 
}

//adding x to skip the test, currently it is failing
func TestCreateUsersWithArrayInput(t *testing.T) {
 s := sw.NewUserApi()
  newUsers := []sw.User{
                         sw.User {
                           Id: int64(1001), 
                           FirstName: "gopher1",
                           LastName : "lang1",
                           Username : "gopher1",
                           Password : "lang1",
                           Email : "lang1@test.com",
                           Phone : "5101112222",
                           UserStatus: int32(1),
                         },
                         sw.User {
                           Id: int64(1002), 
                           FirstName: "gopher2",
                           LastName : "lang2",
                           Username : "gopher2",
                           Password : "lang2",
                           Email : "lang2@test.com",
                           Phone : "5101112222",
                           UserStatus: int32(1),
                         },
                       }
    
 apiResponse, err := s.CreateUsersWithArrayInput(newUsers)

 if err != nil {
   t.Errorf("Error while adding users")
   t.Log(err)
 }
 if apiResponse.Response.StatusCode != 200 {
   t.Log(apiResponse.Response)
 }

 //tear down
 _, err1 := s.DeleteUser("gopher1")
 if(err1 != nil){
       t.Errorf("Error while deleting user")
   t.Log(err1)
 }

 _, err2 := s.DeleteUser("gopher2")
 if(err2 != nil){
       t.Errorf("Error while deleting user")
   t.Log(err2)
 } 
}

func TestGetUserByName(t *testing.T) {
  assert := assert.New(t)

  s := sw.NewUserApi()
  resp, apiResponse, err := s.GetUserByName("gopher")
  if err != nil {
    t.Errorf("Error while getting user by id")
    t.Log(err)
  } else {
    assert.Equal(resp.Id, int64(1000), "User id should be equal")
    assert.Equal(resp.Username, "gopher", "User name should be gopher")
    assert.Equal(resp.LastName, "lang", "Last name should be lang")
    //t.Log(resp)
  }
  if apiResponse.Response.StatusCode != 200 {
    t.Log(apiResponse.Response)
  } 
}

func TestGetUserByNameWithInvalidID(t *testing.T) {
  s := sw.NewUserApi()
  resp, apiResponse, err := s.GetUserByName("999999999")
  if err != nil {
    t.Errorf("Error while getting user by invalid id")
    t.Log(err)
    t.Log(apiResponse)
  } else {
    t.Log(resp)
  }
  if apiResponse.Response.StatusCode != 200 {
    t.Log(apiResponse.Response)
  } 
}

func TestUpdateUser(t *testing.T) {
  assert := assert.New(t)
  s := sw.NewUserApi()
  newUser := sw.User{
    Id: 1000, 
    FirstName: "gopher20",
    LastName : "lang20",
    Username : "gopher",
    Password : "lang",
    Email : "lang@test.com",
    Phone : "5101112222",
    UserStatus: 1}

  apiResponse, err := s.UpdateUser("gopher", newUser)

  if err != nil {
    t.Errorf("Error while deleting user by id")
    t.Log(err)
  }
  if apiResponse.Response.StatusCode != 200 {
    t.Log(apiResponse.Response)
  }

  //verify changings are correct
  resp, apiResponse, err := s.GetUserByName("gopher")
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
  s := sw.NewUserApi()
  apiResponse, err := s.DeleteUser("gopher")

  if err != nil {
    t.Errorf("Error while deleting user")
    t.Log(err)
  }
  if apiResponse.Response.StatusCode != 200 {
    t.Log(apiResponse.Response)
  }
}