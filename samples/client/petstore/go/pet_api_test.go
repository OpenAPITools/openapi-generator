package main

import (
	sw "./go-petstore"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestAddPet(t *testing.T) {
	s := sw.NewPetApi()
	newPet := (sw.Pet{Id: 12830, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"})

	err := s.AddPet(newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
}

func TestGetPetById(t *testing.T) {
	assert := assert.New(t)

	s := sw.NewPetApi()
	resp, err := s.GetPetById(12830)
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
	} else {
		assert.Equal(resp.Id, int64(12830), "Pet id should be equal")
		assert.Equal(resp.Name, "gopher", "Pet name should be gopher")
		assert.Equal(resp.Status, "pending", "Pet status should be pending")

		//t.Log(resp)
	}
}

func TestUpdatePetWithForm(t *testing.T) {
	s := sw.NewPetApi()
	err := s.UpdatePetWithForm(12830, "golang", "available")

	if err != nil {
		t.Errorf("Error while updating pet by id")
		t.Log(err)
	}
}
