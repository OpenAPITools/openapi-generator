package main

import (
	"testing"

	sw "go-petstore"
)

type testCase struct {
	String      string
	ShouldMatch bool
}

func TestJsonCheck(t *testing.T) {
	testCases := []testCase{
		{"application/json", true},
		{"application/vnd.org.application+json", true},
		{"application/hal+json", true},
		{"text/json", true},
		{"text/vnd.org.application+json", true},
		{"text/hal+json", true},

		{"application/bson", false},
		{"application/+json", false},
		{"text/bson", false},
		{"text/+json", false},

		{"zip/json", false},
	}

	for _, c := range testCases {
		actual := sw.JsonCheck.MatchString(c.String)
		if actual != c.ShouldMatch {
			t.Errorf("Expected %s to result in %v but got %v", c.String, c.ShouldMatch, actual)
		}
	}
}

func TestXmlRegex(t *testing.T) {
	testCases := []testCase{
		{"application/xml", true},
		{"application/vnd.org.application+xml", true},
		{"application/hal+xml", true},
		{"text/xml", true},
		{"text/vnd.org.application+xml", true},
		{"text/hal+xml", true},

		{"application/bmx", false},
		{"application/+xml", false},
		{"text/bmx", false},
		{"text/+xml", false},

		{"zip/xml", false},
	}

	for _, c := range testCases {
		actual := sw.XmlCheck.MatchString(c.String)
		if actual != c.ShouldMatch {
			t.Errorf("Expected %s to result in %v but got %v", c.String, c.ShouldMatch, actual)
		}
	}
}
