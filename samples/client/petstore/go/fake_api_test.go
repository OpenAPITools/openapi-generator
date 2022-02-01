package main

import (
	"context"
	"testing"

	sw "github.com/OpenAPITools/openapi-generator/samples/client/petstore/go/go-petstore"
)

// TestPutBodyWithFileSchema ensures a model with the name 'File'
// gets converted properly to the petstore.File struct vs. *os.File
// as specified in typeMapping for 'File'.
func TestPutBodyWithFileSchema(t *testing.T) {
	return // early return to test compilation

	schema := sw.FileSchemaTestClass{
		File:  &sw.File{SourceURI: sw.PtrString("https://example.com/image.png")},
		Files: []sw.File{{SourceURI: sw.PtrString("https://example.com/image.png")}}}

	r, err := client.FakeApi.TestBodyWithFileSchema(context.Background()).Body(schema).Execute()

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
