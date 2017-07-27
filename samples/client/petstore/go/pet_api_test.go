package main

import (
	"fmt"
	"os"
	"testing"

	sw "./go-petstore"
	"github.com/stretchr/testify/assert"
)

var client *sw.APIClient

const testHost = "testhost"

func TestMain(m *testing.M) {
	cfg := sw.NewConfiguration()
	cfg.AddDefaultHeader("testheader", "testvalue")
	cfg.Host = testHost
	client = sw.NewAPIClient(cfg)
	retCode := m.Run()
	os.Exit(retCode)
}

func TestAddPet(t *testing.T) {
	newPet := (sw.Pet{Id: 12830, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestFindPetsByStatusWithMissingParam(t *testing.T) {
	_, r, err := client.PetApi.FindPetsByStatus(nil, nil)

	if err != nil {
		t.Errorf("Error while testing TestFindPetsByStatusWithMissingParam")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestGetPetById(t *testing.T) {
	isPetCorrect(t, 12830, "gopher", "pending")
}

func TestGetPetByIdWithInvalidID(t *testing.T) {
	resp, r, err := client.PetApi.GetPetById(nil, 999999999)
	if r != nil && r.StatusCode == 404 {
		return // This is a pass condition. API will return with a 404 error.
	} else if err != nil {
		t.Errorf("Error while getting pet by invalid id")
		t.Log(err)
		t.Log(r)
	} else {
		t.Log(resp)
	}
}

func TestUpdatePetWithForm(t *testing.T) {
	r, err := client.PetApi.UpdatePetWithForm(nil, 12830, map[string]interface{}{"name": "golang", "status": "available"})

	if err != nil {
		t.Errorf("Error while updating pet by id")
		t.Log(err)
		t.Log(r)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestFindPetsByTag(t *testing.T) {
	var found bool = false
	resp, r, err := client.PetApi.FindPetsByTags(nil, []string{"tag2"})
	if err != nil {
		t.Errorf("Error while getting pet by tag")
		t.Log(err)
		t.Log(r)
	} else {
		if len(resp) == 0 {
			t.Errorf("Error no pets returned")
		} else {

			assert := assert.New(t)
			for i := 0; i < len(resp); i++ {
				if resp[i].Id == 12830 {
					assert.Equal(resp[i].Status, "pending", "Pet status should be `pending`")
					found = true
				}
			}
		}

		if found == false {
			t.Errorf("Error while getting pet by tag could not find 12830")
		}

		if r.StatusCode != 200 {
			t.Log(r)
		}
	}
}

func TestFindPetsByStatus(t *testing.T) {
	resp, r, err := client.PetApi.FindPetsByStatus(nil, []string{"available"})
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
		t.Log(r)
	} else {
		if len(resp) == 0 {
			t.Errorf("Error no pets returned")
		} else {
			assert := assert.New(t)
			for i := 0; i < len(resp); i++ {
				assert.Equal(resp[i].Status, "available", "Pet status should be `available`")
			}
		}

		if r.StatusCode != 200 {
			t.Log(r)
		}
	}
}

func TestUploadFile(t *testing.T) {
	file, _ := os.Open("../python/testfiles/foo.png")

	_, r, err := client.PetApi.UploadFile(nil, 12830, map[string]interface{}{"name": "golang", "file": file})

	if err != nil {
		t.Errorf("Error while uploading file")
		t.Log(err)
	}

	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestDeletePet(t *testing.T) {
	r, err := client.PetApi.DeletePet(nil, 12830, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

/*
// Test we can concurrently create, retrieve, update, and delete.
func TestConcurrency(t *testing.T) {
	errc := make(chan error)

	newPets := []sw.Pet{
		sw.Pet{Id: 912345, Name: "gopherFred", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"},
		sw.Pet{Id: 912346, Name: "gopherDan", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "active"},
		sw.Pet{Id: 912347, Name: "gopherRick", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "mia"},
		sw.Pet{Id: 912348, Name: "gopherJohn", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "active"},
		sw.Pet{Id: 912349, Name: "gopherAlf", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"},
		sw.Pet{Id: 912350, Name: "gopherRob", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"},
		sw.Pet{Id: 912351, Name: "gopherIan", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "active"},
	}

	// Add the pets.
	for _, pet := range newPets {
		go func(newPet sw.Pet) {
			r, err := client.PetApi.AddPet(nil, newPet)
			if r.StatusCode != 200 {
				t.Log(r)
			}
			errc <- err
		}(pet)
	}
	waitOnFunctions(t, errc, len(newPets))

	// Verify they are correct.
	for _, pet := range newPets {
		go func(pet sw.Pet) {
			isPetCorrect(t, pet.Id, pet.Name, pet.Status)
			errc <- nil
		}(pet)
	}

	waitOnFunctions(t, errc, len(newPets))

	// Update all to active with the name gopherDan
	for _, pet := range newPets {
		go func(id int64) {
			r, err := client.PetApi.UpdatePet(nil, sw.Pet{Id: (int64)(id), Name: "gopherDan", PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "active"})
			if r.StatusCode != 200 {
				t.Log(r)
			}
			errc <- err
		}(pet.Id)
	}
	waitOnFunctions(t, errc, len(newPets))

	// Verify they are correct.
	for _, pet := range newPets {
		go func(pet sw.Pet) {
			isPetCorrect(t, pet.Id, "gopherDan", "active")
			errc <- nil
		}(pet)
	}

	waitOnFunctions(t, errc, len(newPets))

	// Delete them all.
	for _, pet := range newPets {
		go func(id int64) {
			deletePet(t, (int64)(id))
			errc <- nil
		}(pet.Id)
	}
	waitOnFunctions(t, errc, len(newPets))
}
*/

func waitOnFunctions(t *testing.T, errc chan error, n int) {
	for i := 0; i < n; i++ {
		err := <-errc
		if err != nil {
			t.Errorf("Error performing concurrent test")
			t.Log(err)
		}
	}
}

func deletePet(t *testing.T, id int64) {
	r, err := client.PetApi.DeletePet(nil, id, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func isPetCorrect(t *testing.T, id int64, name string, status string) {
	assert := assert.New(t)
	resp, r, err := client.PetApi.GetPetById(nil, id)
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
	} else {
		assert.Equal(resp.Id, int64(id), "Pet id should be equal")
		assert.Equal(resp.Name, name, fmt.Sprintf("Pet name should be %s", name))
		assert.Equal(resp.Status, status, fmt.Sprintf("Pet status should be %s", status))

		//t.Log(resp)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
