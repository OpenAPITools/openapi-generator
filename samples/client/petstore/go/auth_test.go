package main

import (
	"net/http"
	"net/http/httputil"
	"strings"
	"testing"
	"time"

	"golang.org/x/net/context"

	"golang.org/x/oauth2"

	sw "./go-petstore"
)

func TestOAuth2(t *testing.T) {
	// Setup some fake oauth2 configuration
	cfg := &oauth2.Config{
		ClientID:     "1234567",
		ClientSecret: "SuperSecret",
		Endpoint: oauth2.Endpoint{
			AuthURL:  "https://devnull",
			TokenURL: "https://devnull",
		},
		RedirectURL: "https://devnull",
	}

	// and a fake token
	tok := oauth2.Token{
		AccessToken:  "FAKE",
		RefreshToken: "So Fake",
		Expiry:       time.Now().Add(time.Hour * 100000),
		TokenType:    "Bearer",
	}

	// then a fake tokenSource
	tokenSource := cfg.TokenSource(createContext(nil), &tok)
	auth := context.WithValue(oauth2.NoContext, sw.ContextOAuth2, tokenSource)

	newPet := (sw.Pet{Id: 12992, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, err := httputil.DumpRequest(r.Request, true)

	if !strings.Contains((string)(reqb), "Authorization: Bearer FAKE") {
		t.Errorf("OAuth2 Authentication is missing")
	}
}

func TestBasicAuth(t *testing.T) {

	auth := context.WithValue(context.TODO(), sw.ContextBasicAuth, sw.BasicAuth{
		UserName: "fakeUser",
		Password: "f4k3p455",
	})

	newPet := (sw.Pet{Id: 12992, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(auth, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, err := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Authorization: Basic ZmFrZVVzZXI6ZjRrM3A0NTU") {
		t.Errorf("Basic Authentication is missing")
	}
}

func TestAccessToken(t *testing.T) {
	auth := context.WithValue(context.TODO(), sw.ContextAccessToken, "TESTFAKEACCESSTOKENISFAKE")

	newPet := (sw.Pet{Id: 12992, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, err := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Authorization: Bearer TESTFAKEACCESSTOKENISFAKE") {
		t.Errorf("AccessToken Authentication is missing")
	}
}

func TestAPIKeyNoPrefix(t *testing.T) {
	auth := context.WithValue(context.TODO(), sw.ContextAPIKey, sw.APIKey{Key: "TEST123"})

	newPet := (sw.Pet{Id: 12992, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	_, r, err = client.PetApi.GetPetById(auth, 12992)
	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}

	reqb, err := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Api_key: TEST123") {
		t.Errorf("APIKey Authentication is missing")
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)
	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestAPIKeyWithPrefix(t *testing.T) {
	auth := context.WithValue(context.TODO(), sw.ContextAPIKey, sw.APIKey{Key: "TEST123", Prefix: "Bearer"})

	newPet := (sw.Pet{Id: 12992, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	_, r, err = client.PetApi.GetPetById(auth, 12992)
	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}

	reqb, err := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Api_key: Bearer TEST123") {
		t.Errorf("APIKey Authentication is missing")
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)
	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestDefaultHeader(t *testing.T) {

	newPet := (sw.Pet{Id: 12992, Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: "pending", Tags: []sw.Tag{sw.Tag{Id: 1, Name: "tag2"}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Errorf("Error while adding pet")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(nil, 12992, nil)

	if err != nil {
		t.Errorf("Error while deleting pet by id")
		t.Log(err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, err := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Testheader: testvalue") {
		t.Errorf("Default Header is missing")
	}
}

func TestHostOverride(t *testing.T) {
	_, r, err := client.PetApi.FindPetsByStatus(nil, nil)

	if err != nil {
		t.Errorf("Error while finding pets by status")
		t.Log(err)
	}

	if r.Request.Host != testHost {
		t.Errorf("Request Host is %v, expected %v", r.Request.Host, testHost)
	}
}

// Add custom clients to the context.
func createContext(httpClient *http.Client) context.Context {
	parent := oauth2.NoContext
	ctx := context.WithValue(parent, oauth2.HTTPClient, httpClient)
	return ctx
}
