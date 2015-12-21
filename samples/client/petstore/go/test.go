package main

import (
	"fmt"
	//    "log"
	swagger "./swagger"
)

func main() {

	fmt.Println("hello world")
	s := swagger.NewPetApi()
	&s.updatePetWithForm("2", "golang", "available")

}
