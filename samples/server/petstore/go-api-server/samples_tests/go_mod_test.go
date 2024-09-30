package main

import (
	"testing"

	"github.com/GIT_USER_ID/GIT_REPO_ID/samples_tests/utils"
)

func Test_Go_Mod(t *testing.T) {

	t.Run("Check module", func(t *testing.T) {

		filepath := "../go.mod"

		lines := utils.ReadLines(filepath)
		expected := "module github.com/GIT_USER_ID/GIT_REPO_ID"

		if lines[0] != expected {
			t.Errorf("Expected '%s', but got '%s'", expected, lines[0])
		}
	})

	t.Run("Check Go version", func(t *testing.T) {

		filepath := "../go.mod"

		lines := utils.ReadLines(filepath)
		expected := "go 1.18"
		
		if lines[2] != expected {
			t.Errorf("Expected '%s', but got '%s'", expected, lines[2])
		}
	})

}
