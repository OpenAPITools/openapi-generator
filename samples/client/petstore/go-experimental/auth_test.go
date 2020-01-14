package main

import (
	"bytes"
	"context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httputil"
	"os"
	"strings"
	"testing"
	"time"

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
	auth := context.WithValue(context.Background(), sw.ContextOAuth2, tokenSource)

	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(context.Background(), newPet)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)

	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, _ := httputil.DumpRequest(r.Request, true)

	if !strings.Contains((string)(reqb), "Authorization: Bearer FAKE") {
		t.Errorf("OAuth2 Authentication is missing")
	}
}

func TestBasicAuth(t *testing.T) {

	auth := context.WithValue(context.Background(), sw.ContextBasicAuth, sw.BasicAuth{
		UserName: "fakeUser",
		Password: "f4k3p455",
	})

	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(auth, newPet)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)

	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, _ := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Authorization: Basic ZmFrZVVzZXI6ZjRrM3A0NTU") {
		t.Errorf("Basic Authentication is missing")
	}
}

func TestAccessToken(t *testing.T) {
	auth := context.WithValue(context.Background(), sw.ContextAccessToken, "TESTFAKEACCESSTOKENISFAKE")

	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)

	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, _ := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Authorization: Bearer TESTFAKEACCESSTOKENISFAKE") {
		t.Errorf("AccessToken Authentication is missing")
	}
}

func TestAPIKeyNoPrefix(t *testing.T) {
	auth := context.WithValue(context.Background(), sw.ContextAPIKeys, map[string]sw.APIKey{"api_key": sw.APIKey{Key: "TEST123"}})

	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(context.Background(), newPet)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	_, r, err = client.PetApi.GetPetById(auth, 12992)
	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}

	reqb, _ := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Api_key: TEST123") {
		t.Errorf("APIKey Authentication is missing")
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)
	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

func TestAPIKeyWithPrefix(t *testing.T) {
	auth := context.WithValue(context.Background(), sw.ContextAPIKeys, map[string]sw.APIKey{"api_key": sw.APIKey{Key: "TEST123", Prefix: "Bearer"}})

	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(nil, newPet)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	_, r, err = client.PetApi.GetPetById(auth, 12992)
	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}

	reqb, _ := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Api_key: Bearer TEST123") {
		t.Errorf("APIKey Authentication is missing")
	}

	r, err = client.PetApi.DeletePet(auth, 12992, nil)
	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
}

// Test RSA private key as published in Appendix C 'Test Values' of
// https://www.ietf.org/id/draft-cavage-http-signatures-12.txt
const rsaTestPrivateKey string = `-----BEGIN RSA PRIVATE KEY-----
MIICXgIBAAKBgQDCFENGw33yGihy92pDjZQhl0C36rPJj+CvfSC8+q28hxA161QF
NUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6Z4UMR7EOcpfdUE9Hf3m/hs+F
UR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJwoYi+1hqp1fIekaxsyQIDAQAB
AoGBAJR8ZkCUvx5kzv+utdl7T5MnordT1TvoXXJGXK7ZZ+UuvMNUCdN2QPc4sBiA
QWvLw1cSKt5DsKZ8UETpYPy8pPYnnDEz2dDYiaew9+xEpubyeW2oH4Zx71wqBtOK
kqwrXa/pzdpiucRRjk6vE6YY7EBBs/g7uanVpGibOVAEsqH1AkEA7DkjVH28WDUg
f1nqvfn2Kj6CT7nIcE3jGJsZZ7zlZmBmHFDONMLUrXR/Zm3pR5m0tCmBqa5RK95u
412jt1dPIwJBANJT3v8pnkth48bQo/fKel6uEYyboRtA5/uHuHkZ6FQF7OUkGogc
mSJluOdc5t6hI1VsLn0QZEjQZMEOWr+wKSMCQQCC4kXJEsHAve77oP6HtG/IiEn7
kpyUXRNvFsDE0czpJJBvL/aRFUJxuRK91jhjC68sA7NsKMGg5OXb5I5Jj36xAkEA
gIT7aFOYBFwGgQAQkWNKLvySgKbAZRTeLBacpHMuQdl1DfdntvAyqpAZ0lY0RKmW
G6aFKaqQfOXKCyWoUiVknQJAXrlgySFci/2ueKlIE1QqIiLSZ8V8OlpFLRnb1pzI
7U1yQXnTAEFYM560yJlzUpOb1V4cScGd365tiSMvxLOvTA==
-----END RSA PRIVATE KEY-----`

func writeTestRsaPemKey(t *testing.T, fileName string) {
	err := ioutil.WriteFile(fileName, []byte(rsaTestPrivateKey), 0644)
	if err != nil {
		t.Fatalf("Error writing private key: %v", err)
	}
}

func writeRandomTestRsaPemKey(t *testing.T, fileName string) {
	key, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		t.Fatalf("Error generating RSA private key file: %v", err)
	}
	var outFile *os.File
	outFile, err = os.Create(fileName)
	if err != nil {
		t.Fatalf("Error creating RSA private key file: %v", err)
	}
	defer outFile.Close()

	var privateKey = &pem.Block{
		Type:  "PRIVATE KEY",
		Bytes: x509.MarshalPKCS1PrivateKey(key),
	}

	err = pem.Encode(outFile, privateKey)
	if err != nil {
		t.Fatalf("Error encoding RSA private key: %v", err)
	}
}

/*
Commented out because OpenAPITools is configured to use golang 1.8 at build time
x509.MarshalPKCS8PrivateKey is not present.
func writeRandomTestEcdsaPemKey(t *testing.T, fileName string) {
	key, err := ecdsa.GenerateKey(elliptic.P521(), rand.Reader)
	if err != nil {
		t.Fatalf("Error generating ECDSA private key file: %v", err)
	}
	var outFile *os.File
	outFile, err = os.Create(fileName)
	if err != nil {
		t.Fatalf("Error creating ECDSA private key file: %v", err)
	}
	defer outFile.Close()

	var keybytes []byte
	keybytes, err = x509.MarshalPKCS8PrivateKey(key)
	if err != nil {
		t.Fatalf("Error marshaling ECDSA private key: %v", err)
	}
	var privateKey = &pem.Block{
		Type:  "PRIVATE KEY",
		Bytes: keybytes,
	}

	err = pem.Encode(outFile, privateKey)
	if err != nil {
		t.Fatalf("Error encoding ECDSA private key: %v", err)
	}
}
*/

func TestHttpSignaturePrivateKeys(t *testing.T) {
	privateKeyPath := "privatekey.pem"
	{
		authConfig := sw.HttpSignatureAuth{
			KeyId:         "my-key-id",
			Algorithm:     "hs2019",
			SignedHeaders: []string{"Content-Type"},
		}
		// Generate test private key.
		writeRandomTestRsaPemKey(t, privateKeyPath)
		err := authConfig.LoadPrivateKey(privateKeyPath)
		if err != nil {
			t.Fatalf("Error loading private key: %v", err)
		}
	}
	/*
		{
			authConfig := sw.HttpSignatureAuth{
				KeyId:         "my-key-id",
				Algorithm:     "hs2019",
				SignedHeaders: []string{"Content-Type"},
			}
			// Generate test private key.
			writeRandomTestEcdsaPemKey(t, privateKeyPath)
			err := authConfig.LoadPrivateKey(privateKeyPath)
			if err != nil {
				t.Fatalf("Error loading private key: %v", err)
			}
		}
	*/
}
func executeHttpSignatureAuth(t *testing.T, authConfig *sw.HttpSignatureAuth) string {
	privateKeyPath := "rsa.pem"
	writeTestRsaPemKey(t, privateKeyPath)
	err := authConfig.LoadPrivateKey(privateKeyPath)
	if err != nil {
		t.Fatalf("Error loading private key: %v", err)
	}
	auth := context.WithValue(context.Background(), sw.ContextHttpSignatureAuth, *authConfig)
	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"},
		Status:    sw.PtrString("pending"),
		Tags:      &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err2 := client.PetApi.AddPet(nil, newPet)

	if err2 != nil {
		t.Fatalf("Error while adding pet: %v", err2)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	_, r, err = client.PetApi.GetPetById(auth, 12992)
	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	// The request should look like this:
	//
	// GET /v2/pet/12992 HTTP/1.1
	// Host: petstore.swagger.io:80
	// Accept: application/json
	// Authorization: Signature keyId="my-key-id", algorithm="hs2019", headers="(request-target) date host digest content-type", signature="RMJZjVVxzlH02wlxiQgUYDe4QxZaI5IJNIfB2BK8Dhbv3WQ2gw0xyqC+5HiKUmT/cfchhhkUNNsUtiVRnjZmFwtSfYxHfiQvH3KWXlLCMwKGNQC3YzD9lnoWdx0pA5Kxbr0/ygmr3+lTyuN2PJg4IS7Ji/AaKAqIZx7RsHS8Bxw="
	// Date: Tue, 14 Jan 2020 06:41:22 GMT
	// Digest: SHA-512=z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==
	// Testheader: testvalue
	// User-Agent: OpenAPI-Generator/1.0.0/go
	reqb, _ := httputil.DumpRequest(r.Request, true)
	reqt := (string)(reqb)
	fmt.Printf("Request with HTTP signature:\n%v\n", reqt)
	var sb bytes.Buffer
	fmt.Fprintf(&sb, `Authorization: Signature keyId="%s", algorithm="%s", headers="`,
		authConfig.KeyId, authConfig.Algorithm)
	for i, header := range authConfig.SignedHeaders {
		header = strings.ToLower(header)
		if i > 0 {
			fmt.Fprintf(&sb, " ")
		}
		fmt.Fprintf(&sb, header)
		switch header {
		case "date":
			if !strings.Contains(reqt, "Date: ") {
				t.Errorf("Date header is incorrect")
			}
		case "digest":
			var prefix string
			switch authConfig.Algorithm {
			case sw.HttpSignatureAlgorithmRsaSha256:
				prefix = "SHA-256="
			default:
				prefix = "SHA-512="
			}
			if !strings.Contains(reqt, fmt.Sprintf("Digest: %s", prefix)) {
				t.Errorf("Digest header is incorrect")
			}
		}
	}
	if len(authConfig.SignedHeaders) == 0 {
		fmt.Fprintf(&sb, sw.HttpSignatureParameterCreated)
	}
	fmt.Fprintf(&sb, `", signature="`)
	if !strings.Contains(reqt, sb.String()) {
		t.Errorf("Authorization header is incorrect")
	}
	return r.Request.Header.Get("Authorization")
}
func TestHttpSignatureAuth(t *testing.T) {
	authConfig := sw.HttpSignatureAuth{
		KeyId:     "my-key-id",
		Algorithm: sw.HttpSignatureAlgorithmHs2019,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			"Host",                                 // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Date",                                 // The date and time at which the message was originated.
			"Content-Type",                         // The Media type of the body of the request.
			"Digest",                               // A cryptographic digest of the request body.
		},
	}
	executeHttpSignatureAuth(t, &authConfig)

	// Test with empty signed headers. The client should automatically add the "(created)" parameter by default.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		Algorithm:     sw.HttpSignatureAlgorithmHs2019,
		SignedHeaders: []string{},
	}
	executeHttpSignatureAuth(t, &authConfig)

	// Test with deprecated RSA-SHA512, some servers may still support it.
	authConfig = sw.HttpSignatureAuth{
		KeyId:     "my-key-id",
		Algorithm: sw.HttpSignatureAlgorithmRsaSha512,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			"Host",                                 // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Date",                                 // The date and time at which the message was originated.
			"Content-Type",                         // The Media type of the body of the request.
			"Digest",                               // A cryptographic digest of the request body.
		},
	}
	executeHttpSignatureAuth(t, &authConfig)

	// Test with deprecated RSA-SHA256, some servers may still support it.
	authConfig = sw.HttpSignatureAuth{
		KeyId:     "my-key-id",
		Algorithm: sw.HttpSignatureAlgorithmRsaSha256,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			"Host",                                 // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Date",                                 // The date and time at which the message was originated.
			"Content-Type",                         // The Media type of the body of the request.
			"Digest",                               // A cryptographic digest of the request body.
		},
	}
	executeHttpSignatureAuth(t, &authConfig)

	// Test with headers without date. This makes it possible to get a fixed signature, used for unit test purpose.
	// This should not be used in production code as it could potentially allow replay attacks.
	authConfig = sw.HttpSignatureAuth{
		KeyId:     "my-key-id",
		Algorithm: sw.HttpSignatureAlgorithmHs2019,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget,
			"Host",         // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Content-Type", // The Media type of the body of the request.
			"Digest",       // A cryptographic digest of the request body.
		},
	}
	authorizationHeaderValue := executeHttpSignatureAuth(t, &authConfig)
	expectedSignature := "ED7P+ETthVNx45FZ6Z1lzNDuIE+QuR05GpNJCg7yVGJGDng98inMiPumgdUiljZZr3aEacHXH2Ln8epF4Op6GRGrNYAjXVwK2Nm/6zxt1EEr2JAfA/L5eNjJq9VfZzPjcdanjfqEKvrY6isPpQLsBwRlazhbITsW5E6hAsbVnGk="
	expectedAuthorizationHeader := fmt.Sprintf(
		`Signature keyId="my-key-id", `+
			`algorithm="hs2019", headers="(request-target) host content-type digest", `+
			`signature="%s"`, expectedSignature)
	if authorizationHeaderValue != expectedAuthorizationHeader {
		t.Errorf("Authorization header value is incorrect. Got '%s' but expected '%s'", authorizationHeaderValue, expectedAuthorizationHeader)
	}
}

func TestDefaultHeader(t *testing.T) {
	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"}, Status: sw.PtrString("pending"),
		Tags: &[]sw.Tag{sw.Tag{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	r, err := client.PetApi.AddPet(context.Background(), newPet)

	if err != nil {
		t.Fatalf("Error while adding pet: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	r, err = client.PetApi.DeletePet(context.Background(), 12992, nil)

	if err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}
	reqb, _ := httputil.DumpRequest(r.Request, true)
	if !strings.Contains((string)(reqb), "Testheader: testvalue") {
		t.Errorf("Default Header is missing")
	}
}

func TestHostOverride(t *testing.T) {
	_, r, err := client.PetApi.FindPetsByStatus(context.Background(), nil)

	if err != nil {
		t.Fatalf("Error while finding pets by status: %v", err)
	}

	if r.Request.URL.Host != testHost {
		t.Errorf("Request Host is %v, expected %v", r.Request.Host, testHost)
	}
}

func TestSchemeOverride(t *testing.T) {
	_, r, err := client.PetApi.FindPetsByStatus(context.Background(), nil)

	if err != nil {
		t.Fatalf("Error while finding pets by status: %v", err)
	}

	if r.Request.URL.Scheme != testScheme {
		t.Errorf("Request Scheme is %v, expected %v", r.Request.URL.Scheme, testScheme)
	}
}

// Add custom clients to the context.
func createContext(httpClient *http.Client) context.Context {
	parent := oauth2.NoContext
	ctx := context.WithValue(parent, oauth2.HTTPClient, httpClient)
	return ctx
}
