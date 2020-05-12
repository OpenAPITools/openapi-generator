package main

import (
	"context"
	"testing"

	sw "./go-petstore"
)

// TestPutBodyWithFileSchema ensures a model with the name 'File'
// gets converted properly to the petstore.File struct vs. *os.File
// as specified in typeMapping for 'File'.
func TestPutBodyWithFileSchema(t *testing.T) {
	return // early return to test compilation

	schema := sw.FileSchemaTestClass{
		File:  &sw.File{SourceURI: sw.PtrString("https://example.com/image.png")},
		Files: &[]sw.File{{SourceURI: sw.PtrString("https://example.com/image.png")}}}

	req := client.FakeApi.TestBodyWithFileSchemaGetRequest(context.Background()).Body(schema)
	r, err := client.FakeApi.TestBodyWithFileSchemaExecute(req)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}
