package main

import (
	sw "./go-petstore"
	"encoding/json"
	"fmt"
)

func main() {

	s := sw.NewPetApi()

	// test POST(body)
	newPet := (sw.Pet{Id: 12830, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending"})

	jsonNewPet, _ := json.Marshal(newPet)
	fmt.Println("newPet:", string(jsonNewPet))
	s.AddPet(newPet)

	// test POST(form)
	s.UpdatePetWithForm(12830, "golang", "available")

	// test GET
	resp, apiResponse, err  := s.GetPetById(12830)
	fmt.Println("GetPetById: ", resp, err, apiResponse)

	err2, apiResponse2 := s.DeletePet(12830, "")
	fmt.Println("DeletePet: ", err2, apiResponse2)
}
