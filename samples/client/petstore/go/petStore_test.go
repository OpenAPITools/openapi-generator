package main

import (
	"testing"

	sw "./swagger"
)

func TestAddPet(t *testing.T) {
	t.Log("Testing TestAddPet...")
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
	t.Log("Testing TestGetPetById...")

	s := sw.NewPetApi()
	resp, err := s.GetPetById(12830)
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
	} else {
		t.Log(resp)
	}
}

func TestUpdatePetWithForm(t *testing.T) {
	t.Log("Testing UpdatePetWithForm...")

	s := sw.NewPetApi()

	err := s.UpdatePetWithForm("12830", "golang", "available")

	if err != nil {
		t.Errorf("Error while updating pet by id")
		t.Log(err)
	} 
}
