package main

import (
	"context"
	"github.com/stretchr/testify/assert"
	"testing"

	sw "go-petstore"
)

const (
	deepObjectURL            = `/v2/fake/deep_object_test?inputOptions[F1]=1&inputOptions[F2]=teststring&inputOptions[F3]=null&inputOptions[id]=1&inputOptions[name]=TestCat&test_pet[F1]=1&test_pet[F2]=teststring&test_pet[F3]=null&test_pet[id]=1&test_pet[name]=Test&test_pet[photoUrls]=http%3A%2F%2Flocalhost&test_pet[tags][F1]=1&test_pet[tags][F2]=teststring&test_pet[tags][F3]=null&test_pet[tags][id]=2&test_pet[tags][name]=tag1`
	paramCollectionFormatURL = `/v2/fake/test-query-parameters?context=context&http=http&ioutil=ioutil&pipe=pipe&url=url`
)

// TestPutBodyWithFileSchema ensures a model with the name 'File'
// gets converted properly to the petstore.File struct vs. *os.File
// as specified in typeMapping for 'File'.
func TestPutBodyWithFileSchema(t *testing.T) {
	return // early return to test compilation

	schema := sw.FileSchemaTestClass{
		File:  &sw.File{SourceURI: sw.PtrString("https://example.com/image.png")},
		Files: []sw.File{{SourceURI: sw.PtrString("https://example.com/image.png")}}}

	r, err := client.FakeAPI.TestBodyWithFileSchema(context.Background()).FileSchemaTestClass(schema).Execute()

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestQueryDeepObject(t *testing.T) {
	req := client.FakeAPI.TestQueryDeepObject(context.Background())

	var id = int64(1)
	var idTag1 = int64(2)
	var nameTag1 = "tag1"
	req = req.TestPet(sw.Pet{
		Id:        &id,
		Name:      "Test",
		PhotoUrls: []string{"http://localhost"},
		Tags: []sw.Tag{
			{
				Id:   &idTag1,
				Name: &nameTag1,
				AdditionalProperties: map[string]interface{}{
					"F1": 1,
					"F2": "teststring",
					"F3": nil,
				},
			},
		},
		AdditionalProperties: map[string]interface{}{
			"F1": 1,
			"F2": "teststring",
			"F3": nil,
		},
	})
	var idcat = int64(1)
	req = req.InputOptions(sw.Category{
		Id:   &idcat,
		Name: "TestCat",
		AdditionalProperties: map[string]interface{}{
			"F1": 1,
			"F2": "teststring",
			"F3": nil,
		},
	})

	r, _ := req.Execute()

	var expectedDeepObjectURL = testScheme + "://" + testHost + deepObjectURL

	assert.Equal(t,
		expectedDeepObjectURL,
		r.Request.URL.String())
}

func TestQueryParameterCollectionFormat(t *testing.T) {
	req := client.FakeAPI.TestQueryParameterCollectionFormat(context.Background()).
		Pipe([]string{"pipe"}).
		Ioutil([]string{"ioutil"}).
		Http([]string{"http"}).
		Url([]string{"url"}).
		Context([]string{"context"})

	r, _ := req.Execute()

	var expectedParamCollectionFormatURL = testScheme + "://" + testHost + paramCollectionFormatURL

	assert.Equal(t,
		expectedParamCollectionFormatURL,
		r.Request.URL.String())
}
