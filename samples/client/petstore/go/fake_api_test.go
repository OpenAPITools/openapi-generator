package main

import (
	"testing"

	sw "./go-petstore"
	"golang.org/x/net/context"
)

// TestPutBodyWithFileSchema ensures a model with the name 'File'
// gets converted properly to the petstore.File struct vs. *os.File
// as specified in typeMapping for 'File'.
func TestPutBodyWithFileSchema(t *testing.T) {
	return // early return to test compilation

	schema := sw.FileSchemaTestClass{
		File:  sw.File{SourceURI: "https://example.com/image.png"},
		Files: []sw.File{{SourceURI: "https://example.com/image.png"}}}

	r, err := client.FakeApi.TestBodyWithFileSchema(context.Background(), schema)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
