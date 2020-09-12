package x_auth_id_alias

import (
	"bytes"
	"context"
	"fmt"
	"io/ioutil"
	"net/http"
	"testing"
)

// RoundTripFunc .
type RoundTripFunc func(req *http.Request) *http.Response

// RoundTrip calls f on given request.
func (f RoundTripFunc) RoundTrip(req *http.Request) (*http.Response, error) {
	return f(req), nil
}

//NewTestClient returns *http.Client with replaced Transport layer.
func NewTestClient(fn RoundTripFunc) *http.Client {
	return &http.Client{
		Transport: RoundTripFunc(fn),
	}
}

type APIRequest interface {
	Execute() (map[string]interface{}, *http.Response, error)
}

func TestAPIKeys(t *testing.T) {
	ctx := context.WithValue(
		context.Background(),
		ContextAPIKeys,
		map[string]APIKey{
			"api_key": {
				Key:    "SECRET_VALUE",
				Prefix: "PREFIX",
			},
		},
	)
	configuration := NewConfiguration()
	apiClient := NewAPIClient(configuration)

	testCases := map[string]struct {
		Check   func(t *testing.T, req *http.Request)
		Request APIRequest
	}{
		"AnyKey": {
			Check: func(t *testing.T, req *http.Request) {
				if req.Header.Get("X-Api-Key") != "PREFIX SECRET_VALUE" {
					t.Fatalf("%s != %s", req.Header.Get("X-Api-Key"), "PREFIX SECRET_VALUE")
				}
				values := req.URL.Query()
				if values.Get("api_key") != "SECRET_VALUE" {
					t.Fatalf("%s != %s", values.Get("api_key"), "SECRET_VALUE")
				}
			},
			Request: apiClient.UsageApi.AnyKey(ctx),
		},
		"BothKeys": {
			Check: func(t *testing.T, req *http.Request) {
				if req.Header.Get("X-Api-Key") != "PREFIX SECRET_VALUE" {
					t.Fatalf("%s != %s", req.Header.Get("X-Api-Key"), "PREFIX SECRET_VALUE")
				}
				values := req.URL.Query()
				if values.Get("api_key") != "SECRET_VALUE" {
					t.Fatalf("%s != %s", values.Get("api_key"), "SECRET_VALUE")
				}
			},
			Request: apiClient.UsageApi.BothKeys(ctx),
		},
		"KeyInHeader": {
			Check: func(t *testing.T, req *http.Request) {
				if req.Header.Get("X-Api-Key") != "PREFIX SECRET_VALUE" {
					t.Fatalf("%s != %s", req.Header.Get("X-Api-Key"), "PREFIX SECRET_VALUE")
				}
				values := req.URL.Query()
				if values.Get("api_key") != "" {
					t.Fatalf("%s != %s", values.Get("api_key"), "")
				}
			},
			Request: apiClient.UsageApi.KeyInHeader(ctx),
		},
		"KeyInQuery": {
			Check: func(t *testing.T, req *http.Request) {
				if req.Header.Get("X-Api-Key") != "" {
					t.Fatalf("%s != %s", req.Header.Get("X-Api-Key"), "")
				}
				values := req.URL.Query()
				if values.Get("api_key") != "SECRET_VALUE" {
					t.Fatalf("%s != %s", values.Get("api_key"), "SECRET_VALUE")
				}
			},
			Request: apiClient.UsageApi.KeyInQuery(ctx),
		},
	}

	for name, tc := range testCases {
		t.Run(name, func(t *testing.T) {
			configuration.HTTPClient = NewTestClient(func(req *http.Request) *http.Response {
				tc.Check(t, req)
				return &http.Response{
					StatusCode: 200,
					Body:       ioutil.NopCloser(bytes.NewBufferString("")),
					Header:     make(http.Header),
				}
			})
			_, httpresp, err := tc.Request.Execute()
			if httpresp.StatusCode != 200 {
				fmt.Printf("%v != 200", httpresp.StatusCode)
				t.FailNow()
			}
			if err != nil {
				fmt.Printf("%v, error: %v", httpresp.Body, err)
				t.FailNow()
			}
		})
	}
}
