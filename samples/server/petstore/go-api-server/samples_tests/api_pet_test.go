package main

import (
	"strings"
	"testing"

	"github.com/GIT_USER_ID/GIT_REPO_ID/samples_tests/utils"
)

func Test_API_Pet(t *testing.T) {

	t.Run("Check DeletePet route exists", func(t *testing.T) {

		filepath := "../go/api_pet.go"

		expected := ("\t\t\"DeletePet\": Route{\n" +
			"\t\t\tstrings.ToUpper(\"Delete\"),\n" +
			"\t\t\t\"/v2/pet/{petId}\",\n" +
			"\t\t\tc.DeletePet,\n" +
			"\t\t}")

		if !strings.Contains(utils.ReadContent(filepath), expected) {
			t.Errorf("Route was not found in the file")
		}
	})
}
