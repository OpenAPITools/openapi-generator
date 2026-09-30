package main

import (
	"testing"

	"github.com/GIT_USER_ID/GIT_REPO_ID/samples_tests/utils"
)

func Test_Model_Pet(t *testing.T) {

	t.Run("Check Pet model exists", func(t *testing.T) {

		filepath := "../go/model_pet.go"

		lines := utils.ReadLines(filepath)
		expected := "\tId int64 `json:\"id,omitempty\"`"

		found := false
		for _, line := range lines {
			if line == expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected line '%s' not found in %s", expected, filepath)
		}
	})
}
