package main

import (
	sw "./go-petstore"
	"github.com/stretchr/testify/assert"
	"os"
	"testing"
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
	assert := assert.New(t)

	s := sw.NewPetApi()
	resp, apiResponse, err := s.GetPetById(12830)
	if err != nil {
		t.Errorf("Error while getting pet by id")
		t.Log(err)
	} else {
		assert.Equal(resp.Id, int64(12830), "Pet id should be equal")
		assert.Equal(resp.Name, "gopher", "Pet name should be gopher")
		assert.Equal(resp.Status, "pending", "Pet status should be pending")

		//t.Log(resp)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
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
	apiResponse, err := s.UpdatePetWithForm(12830, "golang", "available")

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

	_, apiResponse, err := s.UploadFile(12830, "golang", file)

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
	apiResponse, err := s.DeletePet(12830, "")

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if apiResponse.Response.StatusCode != 200 {
		t.Log(apiResponse.Response)
	}
}
