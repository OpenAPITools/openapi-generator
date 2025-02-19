package main

import (
	"testing"

	sw "github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore"

	"github.com/stretchr/testify/assert"
)

func TestRequiredFieldsWithAdditionalPropertiesFalse(t *testing.T) {
	assert := assert.New(t)

	newAnimal := (sw.Animal{})
	jsonAnimal := `{"className":"invalidAnimal","extraThing":"foo"}`

	err := newAnimal.UnmarshalJSON([]byte(jsonAnimal))
	expected := "json: unknown field \"extraThing\""

	assert.ErrorContains(err, expected, "Animal should return error when missing additional fields are present")
}
