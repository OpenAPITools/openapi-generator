package main

import (
	"testing"
	"strings"

	"github.com/GIT_USER_ID/GIT_REPO_ID/samples_tests/utils"
)

func Test_Routers(t *testing.T) {

	t.Run("Check struct Route exists", func(t *testing.T) {

		filepath := "../go/routers.go"

		expected := ("type Route struct {\n" +
			"\tMethod\t  string\n" +
			"\tPattern\t string\n" +
			"\tHandlerFunc http.HandlerFunc\n" +
			"}")

		if !strings.Contains(utils.ReadContent(filepath), expected) {
			t.Errorf("Type Route was not found in the file")
		}
	})

	t.Run("Check map Routes exists", func(t *testing.T) {

		filepath := "../go/routers.go"

		lines := utils.ReadLines(filepath)
		expected := "type Routes map[string]Route"

		if lines[34] != expected {
			t.Errorf("Expected '%s', but got '%s'", expected, lines[34])
		}
	})
}
