package main

import (
	sw "./swagger"
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
	s.UpdatePetWithForm("12830", "golang", "available")

	// test GET
	resp, err := s.GetPetById(12830)
	fmt.Println("GetPetById: ", resp, err)

}
