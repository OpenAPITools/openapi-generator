package main

import (
	sw "./swagger"
	"fmt"
)

func main() {

	fmt.Println("hello world")
	s := sw.NewPetApi()
	//s.UpdatePetWithForm("2", "golang", "available")

	// test Get
	resp, err := s.GetPetById(3)
	fmt.Println("GetPetById: ", resp, err)

}
