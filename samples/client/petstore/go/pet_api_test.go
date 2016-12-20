package main

import (
	"fmt"
	"os"
	"testing"

	"time"

	sw "./go-petstore"
	"github.com/stretchr/testify/assert"
)

func TestAddPet(t *testing.T) {
	s := sw.NewPetApi()
	newPet := (sw.Pet{Id: 12830, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"})

	apiResponse, err := s.AddPet(newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}

func TestFindPetsByStatusWithMissingParam(t *testing.T) {
	s := sw.NewPetApi()

	_, apiResponse, err := s.FindPetsByStatus(nil)

	if err != nil {
		t.Errorf("Error while testing TestFindPetsByStatusWithMissingParam")
		t.Log(err)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse)
	}
}

func TestGetPetById(t *testing.T) {
	s := sw.NewPetApi()
	isPetCorrect(t, s, 12830, "gopher", "pending")
}

func TestGetPetByIdWithInvalidID(t *testing.T) {
	s := sw.NewPetApi()
	resp, apiResponse, err := s.GetPetById(999999999)
	if err != nil {
		t.Errorf("Error while getting pet by invalid id")
		t.Log(err)
		t.Log(apiResponse)
	} else {
		t.Log(resp)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}

func TestUpdatePetWithForm(t *testing.T) {
	s := sw.NewPetApi()
	apiResponse, err := s.UpdatePetWithForm(12830, map[string]interface{}{
		"name":   "golang",
		"status": "available",
	})

	if err != nil {
		t.Errorf("Error while updating pet by id")
		t.Log(err)
		t.Log(apiResponse)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}

func TestFindPetsByStatus(t *testing.T) {
	s := sw.NewPetApi()
	resp, apiResponse, err := s.FindPetsByStatus([]string{"available"})
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
		t.Log(apiResponse)
	} else {
		if len(resp) == 0 {
			t.Errorf("Error no pets returned")
		} else {
			assert := assert.New(t)
			for i := 0; i < len(resp); i++ {
				assert.Equal(resp[i].Status, "available", "Pet status should be `available`")
			}
		}

		if apiResponse.Response.StatusCode != 200 {
			t.Log(apiResponse.Response)
		}
	}
}

func TestUploadFile(t *testing.T) {
	s := sw.NewPetApi()
	file, _ := os.Open("../python/testfiles/foo.png")

	_, apiResponse, err := s.UploadFile(12830, map[string]interface{}{
		"additionalMetadata": "golang",
		"file":               file,
	})

	if err != nil {
		t.Errorf("Error while uploading file")
		t.Log(err)
	}

	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}

func TestDeletePet(t *testing.T) {
	s := sw.NewPetApi()
	deletePet(t, s, 12830)
}

func TestConcurrency(t *testing.T) {
	s := sw.NewPetApi()
	errc := make(chan error)

	newPets := []sw.Pet{sw.Pet{Id: 12345, Name: "gopherFred", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"},
		sw.Pet{Id: 12346, Name: "gopherDan", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "active"},
		sw.Pet{Id: 12347, Name: "gopherRick", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "mia"},
		sw.Pet{Id: 12348, Name: "gopherJohn", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "dead"}}

	// Add the pets at the same time.
	for _, pet := range newPets {
		go func(newPet sw.Pet) {
			apiResponse, err := s.AddPet(newPet)
			if apiResponse.Response.StatusCode != 200 {
				t.Log(apiResponse.Response)
			}
			errc <- err
		}(pet)
	}

	// Wait for the go routines to finish
	for i := 0; i < len(newPets); i++ {
		err := <-errc
		if err != nil {
			t.Errorf("Error while adding pet")
			t.Log(err)
		}
	}

	// Sleep for just a moment to let the backend catch up
	// Getting random 404 errors from the backend
	time.Sleep(time.Second)

	// Verify they are correct.
	isPetCorrect(t, s, 12345, "gopherFred", "pending")
	isPetCorrect(t, s, 12346, "gopherDan", "active")
	isPetCorrect(t, s, 12347, "gopherRick", "mia")
	isPetCorrect(t, s, 12348, "gopherJohn", "dead")

	// Get rid of them.
	deletePet(t, s, 12345)
	deletePet(t, s, 12346)
	deletePet(t, s, 12347)
	deletePet(t, s, 12348)
}

func deletePet(t *testing.T, s *sw.PetApi, id int64) {
	apiResponse, err := s.DeletePet(id, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}

func isPetCorrect(t *testing.T, s *sw.PetApi, id int64, name string, status string) {
	assert := assert.New(t)
	resp, apiResponse, err := s.GetPetById(id)
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
	} else {
		assert.Equal(resp.Id, int64(id), "Pet id should be equal")
		assert.Equal(resp.Name, name, fmt.Sprintf("Pet name should be %s", name))
		assert.Equal(resp.Status, status, fmt.Sprintf("Pet status should be %s", status))

		//t.Log(resp)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}
