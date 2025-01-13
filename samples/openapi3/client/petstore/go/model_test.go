package main

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"

	sw "go-petstore"

	"github.com/stretchr/testify/assert"
)

func TestBanana(t *testing.T) {
	assert := assert.New(t)

	lengthCm := float32(2.3)
	lengthCm2 := float32(2.4)
	newBanana := (sw.Banana{LengthCm: &lengthCm})
	newBanana.LengthCm = &lengthCm2
	assert.Equal(newBanana.LengthCm, &lengthCm2, "Banana LengthCm should be equal")

	// test additional properties
	jsonBanana := `{"fruits":["apple","peach"],"lengthCm":3.1}`
	jsonMap := make(map[string]interface{})
	json.Unmarshal([]byte(jsonBanana), &jsonMap)

	newBanana2 := (sw.Banana{})
	lengthCm3 := float32(3.1)
	json.Unmarshal([]byte(jsonBanana), &newBanana2)
	assert.Equal(newBanana2.LengthCm, &lengthCm3, "Banana2 LengthCm should be equal")
	assert.Equal(newBanana2.AdditionalProperties["fruits"], jsonMap["fruits"], "Banana2 AdditionalProperties should be equal")

	newBanana2Json, _ := json.Marshal(newBanana2)
	assert.Equal(string(newBanana2Json), jsonBanana, "Banana2 JSON string should be equal")
}

func TestDog(t *testing.T) {
	assert := assert.New(t)
	newDog := (sw.Dog{})
	jsonDog := `{"breed":"Shepherd","className":"dog","color":"white"}`
	json.Unmarshal([]byte(jsonDog), &newDog)
	breedString := "Shepherd"
	//assert.Nil(newDog)
	assert.Equal(*newDog.Breed, breedString, "Breed should be `Shepherd`")
}

func TestReadOnlyFirst(t *testing.T) {
	assert := assert.New(t)

	newReadOnlyFirst := (sw.ReadOnlyFirst{Bar: sw.PtrString("Bar value"), Baz: sw.PtrString("Baz value")})
	json, _ := newReadOnlyFirst.MarshalJSON()
	expected := `{"bar":"Bar value","baz":"Baz value"}`

	assert.Equal(expected, (string)(json), "ReadOnlyFirst JSON is incorrect")
}

func TestRequiredFieldsAreValidated(t *testing.T) {
	assert := assert.New(t)

	newPet := (sw.Pet{})
	jsonPet := `{"foo": "Foo value"}`
	err := newPet.UnmarshalJSON([]byte(jsonPet))
	expected := "no value given for required property"

	assert.ErrorContains(err, expected, "Pet should return error when missing required fields")
}

func TestRequiredAnyOfMarshalling(t *testing.T) {
	// Given
	bodyBuf := &bytes.Buffer{}
	bananaLengthCm := float32(23.4)
	req := &sw.FruitJuice{Fruit: sw.GmFruit{
		Banana: &sw.Banana{LengthCm: &bananaLengthCm},
	}}

	// When
	err := json.NewEncoder(bodyBuf).Encode(req)

	// Then
	assert.Nil(t, err)
	assert.Equal(t, strings.TrimSpace(bodyBuf.String()), "{\"fruit\":{\"lengthCm\":23.4}}")
}
