package main

import (
	"context"
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	sw "./go-petstore"
)

var client *sw.APIClient

const testHost = "petstore.swagger.io:80"
const testScheme = "http"

func TestMain(m *testing.M) {
	cfg := sw.NewConfiguration()
	cfg.AddDefaultHeader("testheader", "testvalue")
	cfg.Host = testHost
	cfg.Scheme = testScheme
	client = sw.NewAPIClient(cfg)
	retCode := m.Run()
	os.Exit(retCode)
}

func TestAddPet(t *testing.T) {
	newPet := (sw.Pet{Id: sw.PtrInt64(12830), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(context.Background()).Pet(newPet).Execute()

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestFindPetsByStatusWithMissingParam(t *testing.T) {
	_, r, err := client.PetApi.FindPetsByStatus(context.Background()).Status(nil).Execute()

	if err != nil {
		t.Fatalf("Error while testing TestFindPetsByStatusWithMissingParam: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestGetPetById(t *testing.T) {
	isPetCorrect(t, 12830, "gopher", "pending")
}

func TestGetPetByIdWithInvalidID(t *testing.T) {
	resp, r, err := client.PetApi.GetPetById(context.Background(), 999999999).Execute()
	if r != nil && r.StatusCode == 404 {
		assertedError, ok := err.(sw.GenericOpenAPIError)
		a := assert.New(t)
		a.True(ok)
		a.Contains(string(assertedError.Body()), "type")

		a.Contains(assertedError.Error(), "Not Found")
	} else if err != nil {
		t.Fatalf("Error while getting pet by invalid id: %v", err)
		t.Log(r)
	} else {
		t.Log(resp)
	}
}

func TestUpdatePetWithForm(t *testing.T) {
	r, err := client.PetApi.UpdatePetWithForm(context.Background(), 12830).Name("golang").Status("available").Execute()
	if err != nil {
		t.Fatalf("Error while updating pet by id: %v", err)
		t.Log(r)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	// get the pet with id 12830 from server to verify the update
	isPetCorrect(t, 12830, "golang", "available")
}

func TestFindPetsByTag(t *testing.T) {
	var found = false
	resp, r, err := client.PetApi.FindPetsByTags(context.Background()).Tags([]string{"tag2"}).Execute()
	if err != nil {
		t.Fatalf("Error while getting pet by tag: %v", err)
		t.Log(r)
	} else {
		if len(resp) == 0 {
			t.Errorf("Error no pets returned")
		} else {

			assert := assert.New(t)
			for i := 0; i < len(resp); i++ {
				if *resp[i].Id == 12830 {
					assert.Equal(*resp[i].Status, "available", "Pet status should be `pending`")
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
	resp, r, err := client.PetApi.FindPetsByStatus(context.Background()).Status([]string{"available"}).Execute()
	if err != nil {
		t.Fatalf("Error while getting pet by id: %v", err)
		t.Log(r)
	} else {
		if len(resp) == 0 {
			t.Errorf("Error no pets returned")
		} else {
			assert := assert.New(t)
			for i := 0; i < len(resp); i++ {
				assert.Equal(*resp[i].Status, "available", "Pet status should be `available`")
			}
		}

		if r.StatusCode != 200 {
			t.Log(r)
		}
	}
}

func TestUploadFile(t *testing.T) {
	file, err1 := os.Open("testfiles/foo.png")
	if err1 != nil {
		t.Fatalf("Error opening file: %v", err1)
	}

	_, r, err := client.PetApi.UploadFile(context.Background(), 12830).AdditionalMetadata("golang").File(file).Execute()

	if err != nil {
		t.Fatalf("Error while uploading file: %v", err)
	}

	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestUploadFileRequired(t *testing.T) {
	return // remove when server supports this endpoint
	file, err1 := os.Open("testfiles/foo.png")
	if err1 != nil {
		t.Fatalf("Error opening file: %v", err1)
	}

	_, r, err := client.PetApi.UploadFileWithRequiredFile(context.Background(), 12830).RequiredFile(file).AdditionalMetadata("golang").Execute()

	if err != nil {
		t.Fatalf("Error while uploading file: %v", err)
	}

	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestDeletePet(t *testing.T) {
	r, err := client.PetApi.DeletePet(context.Background(), 12830).Execute()

	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
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
			t.Fatalf("Error performing concurrent test: %v", err)
		}
	}
}

func deletePet(t *testing.T, id int64) {
	r, err := client.PetApi.DeletePet(context.Background(), id).Execute()

	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func isPetCorrect(t *testing.T, id int64, name string, status string) {
	assert := assert.New(t)
	resp, r, err := client.PetApi.GetPetById(context.Background(), id).Execute()
	if err != nil {
		t.Fatalf("Error while getting pet by id: %v", err)
	} else {
		assert.Equal(*resp.Id, int64(id), "Pet id should be equal")
		assert.Equal(resp.Name, name, fmt.Sprintf("Pet name should be %s", name))
		assert.Equal(*resp.Status, status, fmt.Sprintf("Pet status should be %s", status))

		//t.Log(resp)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
