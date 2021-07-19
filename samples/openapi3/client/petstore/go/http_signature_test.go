/*
 * OpenAPI Petstore
 *
 * Unit Tests for HTTP signature.
 */

package main

import (
	"bytes"
	"context"
	"crypto"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/asn1"
	"encoding/base64"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"math/big"
	"net/http"
	"net/http/httputil"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	sw "go-petstore"
)

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

func writeTestRsaPemKey(t *testing.T, filePath string) {
	err := ioutil.WriteFile(filePath, []byte(rsaTestPrivateKey), 0644)
	if err != nil {
		t.Fatalf("Error writing private key: %v", err)
	}
}

type keyFormat int // The serialization format of the private key.

const (
	keyFormatPem      keyFormat = iota // Private key is serialized in PEM format.
	keyFormatPkcs8Pem                  // Private key is serialized as PKCS#8 encoded in PEM format.
	keyFormatPkcs8Der                  // Private key is serialized as PKCS#8 encoded in DER format.
)

func writeRandomTestRsaPemKey(t *testing.T, filePath string, bits int, format keyFormat, passphrase string, alg *x509.PEMCipher) {
	key, err := rsa.GenerateKey(rand.Reader, bits)
	if err != nil {
		t.Fatalf("Error generating RSA private key file: %v", err)
	}
	var outFile *os.File
	outFile, err = os.Create(filePath)
	if err != nil {
		t.Fatalf("Error creating RSA private key file: %v", err)
	}
	defer outFile.Close()
	var privKeyBytes []byte
	switch format {
	case keyFormatPem:
		if passphrase != "" {
			t.Fatalf("Encrypting PKCS#1-encoded private key with passphrase is not supported")
		}
		privKeyBytes = x509.MarshalPKCS1PrivateKey(key)
	case keyFormatPkcs8Pem:
		privKeyBytes, err = x509.MarshalPKCS8PrivateKey(key)
		if err != nil {
			t.Fatalf("Error writing private key: %v", err)
		}
	case keyFormatPkcs8Der:
		if passphrase != "" {
			t.Fatalf("Encrypting DER-encoded private key with passphrase is not supported")
		}
		privKeyBytes, err = x509.MarshalPKCS8PrivateKey(key)
		if err != nil {
			t.Fatalf("Error writing private key: %v", err)
		}
		_, err = outFile.Write(privKeyBytes)
		if err != nil {
			t.Fatalf("Error writing DER-encoded private key: %v", err)
		}
	default:
		t.Fatalf("Unsupported key format: %v", format)
	}

	switch format {
	case keyFormatPem, keyFormatPkcs8Der:
		var pemBlock *pem.Block
		if passphrase == "" {
			pemBlock = &pem.Block{
				Type:  "RSA PRIVATE KEY",
				Bytes: privKeyBytes,
			}
		} else {
			pemBlock, err = x509.EncryptPEMBlock(rand.Reader, "ENCRYPTED PRIVATE KEY", privKeyBytes, []byte(passphrase), *alg)
			if err != nil {
				t.Fatalf("Error encoding RSA private key: %v", err)
			}
		}
		err = pem.Encode(outFile, pemBlock)
		if err != nil {
			t.Fatalf("Error encoding RSA private key: %v", err)
		}
	}
	fmt.Printf("Wrote private key '%s'\n", filePath)
}

func writeRandomTestEcdsaPemKey(t *testing.T, filePath string) {
	key, err := ecdsa.GenerateKey(elliptic.P521(), rand.Reader)
	if err != nil {
		t.Fatalf("Error generating ECDSA private key file: %v", err)
	}
	var outFile *os.File
	outFile, err = os.Create(filePath)
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

// TestHttpSignaturePrivateKeys creates private keys of various sizes, serialization format,
// clear-text and password encrypted.
// Test unmarshaling of the private key.
func TestHttpSignaturePrivateKeys(t *testing.T) {
	var err error
	var dir string
	dir, err = ioutil.TempDir("", "go-http-sign")
	if err != nil {
		t.Fatalf("Failed to create temporary directory")
	}
	defer os.RemoveAll(dir)

	pemCiphers := []x509.PEMCipher{
		x509.PEMCipherDES,
		x509.PEMCipher3DES,
		x509.PEMCipherAES128,
		x509.PEMCipherAES192,
		x509.PEMCipherAES256,
	}
	// Test RSA private keys with various key lengths.
	for _, bits := range []int{1024, 2048, 3072, 4096} {

		for _, format := range []keyFormat{keyFormatPem, keyFormatPkcs8Pem, keyFormatPkcs8Der} {
			// Generate test private key.
			var privateKeyPath string
			switch format {
			case keyFormatPem, keyFormatPkcs8Pem:
				privateKeyPath = "privatekey.pem"
			case keyFormatPkcs8Der:
				privateKeyPath = "privatekey.der"
			default:
				t.Fatalf("Unsupported private key format: %v", format)
			}
			privateKeyPath = filepath.Join(dir, privateKeyPath)
			// Generate keys in PEM format.
			writeRandomTestRsaPemKey(t, privateKeyPath, bits, format, "", nil)

			authConfig := sw.HttpSignatureAuth{
				KeyId:          "my-key-id",
				PrivateKeyPath: privateKeyPath,
				Passphrase:     "",
				SigningScheme:  "hs2019",
				SignedHeaders:  []string{"Content-Type"},
			}

			// Create a context with the HTTP signature configuration parameters.
			_, err = authConfig.ContextWithValue(context.Background())
			if err != nil {
				t.Fatalf("Error loading private key '%s': %v", privateKeyPath, err)
			}

			authConfig = sw.HttpSignatureAuth{
				KeyId:          "my-key-id",
				PrivateKeyPath: privateKeyPath,
				Passphrase:     "my-secret-passphrase",
				SigningScheme:  "hs2019",
				SignedHeaders:  []string{"Content-Type"},
			}
			switch format {
			case keyFormatPem:
				// Do nothing. Keys cannot be encrypted when using PKCS#1.
			case keyFormatPkcs8Pem:
				for _, alg := range pemCiphers {
					writeRandomTestRsaPemKey(t, privateKeyPath, bits, format, authConfig.Passphrase, &alg)
					_, err := authConfig.ContextWithValue(context.Background())
					if err != nil {
						t.Fatalf("Error loading private key '%s': %v", privateKeyPath, err)
					}
				}
			}
		}
	}

	{
		privateKeyPath := "privatekey.pem"
		authConfig := sw.HttpSignatureAuth{
			KeyId:          "my-key-id",
			PrivateKeyPath: privateKeyPath,
			SigningScheme:  "hs2019",
			SignedHeaders:  []string{"Content-Type"},
		}
		// Generate test private key.
		writeRandomTestEcdsaPemKey(t, privateKeyPath)
		_, err := authConfig.ContextWithValue(context.Background())
		if err != nil {
			t.Fatalf("Error loading private key '%s': %v", privateKeyPath, err)
		}
	}
}

func executeHttpSignatureAuth(t *testing.T, authConfig *sw.HttpSignatureAuth, expectSuccess bool) string {
	var err error
	var dir string
	dir, err = ioutil.TempDir("", "go-http-sign")
	if err != nil {
		t.Fatalf("Failed to create temporary directory")
	}
	defer os.RemoveAll(dir)

	cfg := sw.NewConfiguration()
	cfg.AddDefaultHeader("testheader", "testvalue")
	cfg.AddDefaultHeader("Content-Type", "application/json")
	cfg.Host = testHost
	cfg.Scheme = testScheme
	apiClient := sw.NewAPIClient(cfg)

	privateKeyPath := filepath.Join(dir, "rsa.pem")
	writeTestRsaPemKey(t, privateKeyPath)
	authConfig.PrivateKeyPath = privateKeyPath
	var authCtx context.Context
	authCtx, err = authConfig.ContextWithValue(context.Background())
	if expectSuccess && err != nil {
		t.Fatalf("Error validating HTTP signature configuration: %v", err)
	}
	if !expectSuccess && err != nil {
		// Do not continue. Error is expected.
		return ""
	}
	newPet := (sw.Pet{Id: sw.PtrInt64(12992), Name: "gopher",
		PhotoUrls: []string{"http://1.com", "http://2.com"},
		Status:    sw.PtrString("pending"),
		Tags:      []sw.Tag{{Id: sw.PtrInt64(1), Name: sw.PtrString("tag2")}}})

	fmt.Printf("Request with HTTP signature. Scheme: '%s'. Algorithm: '%s'. MaxValidity: %v. Headers: '%v'\n",
		authConfig.SigningScheme, authConfig.SigningAlgorithm, authConfig.SignatureMaxValidity, authConfig.SignedHeaders)

	r, err2 := apiClient.PetApi.AddPet(authCtx).Pet(newPet).Execute()
	if expectSuccess && err2 != nil {
		t.Fatalf("Error while adding pet: %v", err2)
	}
	if !expectSuccess {
		if err2 == nil {
			t.Fatalf("Error was expected, but no error was generated")
		} else {
			// Do not continue. Error is expected.
			return ""
		}
	}
	if r.StatusCode != 200 {
		t.Log(r)
	}

	_, r, err = apiClient.PetApi.GetPetById(authCtx, 12992).Execute()
	if expectSuccess && err != nil {
		t.Fatalf("Error while deleting pet by id: %v", err)
	}

	// The request should look like this:
	//
	// GET /v2/pet/12992 HTTP/1.1
	// Host: petstore.swagger.io:80
	// Accept: application/json
	// Authorization: Signature keyId="my-key-id",algorithm="hs2019",created=1579033245,headers="(request-target) date host digest content-type",signature="RMJZjVVxzlH02wlxiQgUYDe4QxZaI5IJNIfB2BK8Dhbv3WQ2gw0xyqC+5HiKUmT/cfchhhkUNNsUtiVRnjZmFwtSfYxHfiQvH3KWXlLCMwKGNQC3YzD9lnoWdx0pA5Kxbr0/ygmr3+lTyuN2PJg4IS7Ji/AaKAqIZx7RsHS8Bxw="
	// Date: Tue, 14 Jan 2020 06:41:22 GMT
	// Digest: SHA-512=z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==
	// Testheader: testvalue
	// User-Agent: OpenAPI-Generator/1.0.0/go
	reqb, _ := httputil.DumpRequest(r.Request, true)
	reqt := (string)(reqb)
	fmt.Printf("REQUEST:\n%v\n", reqt)
	var sb bytes.Buffer
	fmt.Fprintf(&sb, `Signature keyId="%s",algorithm="%s",`,
		authConfig.KeyId, authConfig.SigningScheme)
	if len(authConfig.SignedHeaders) == 0 {
		fmt.Fprintf(&sb, `created=[0-9]+,`)
	} else {
		for _, header := range authConfig.SignedHeaders {
			header = strings.ToLower(header)
			if header == sw.HttpSignatureParameterCreated {
				fmt.Fprintf(&sb, `created=[0-9]+,`)
			}
			if header == sw.HttpSignatureParameterExpires {
				fmt.Fprintf(&sb, `expires=[0-9]+\.[0-9]{3},`)
			}
		}
	}
	fmt.Fprintf(&sb, `headers="`)
	for i, header := range authConfig.SignedHeaders {
		header = strings.ToLower(header)
		if i > 0 {
			fmt.Fprintf(&sb, " ")
		}
		fmt.Fprintf(&sb, regexp.QuoteMeta(header))
		switch header {
		case "date":
			if !strings.Contains(reqt, "Date: ") {
				t.Errorf("Date header is incorrect")
			}
		case "digest":
			var prefix string
			switch authConfig.SigningScheme {
			case sw.HttpSigningSchemeRsaSha256:
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
		fmt.Fprintf(&sb, regexp.QuoteMeta(sw.HttpSignatureParameterCreated))
	}
	fmt.Fprintf(&sb, `",signature="[a-zA-Z0-9+/]+="`)
	re := regexp.MustCompile(sb.String())
	actual := r.Request.Header.Get("Authorization")
	if !re.MatchString(actual) {
		t.Errorf("Authorization header is incorrect. Expected regex\n'%s'\nbut got\n'%s'", sb.String(), actual)
	}

	validateHttpAuthorizationSignature(t, authConfig, r)
	return r.Request.Header.Get("Authorization")
}

var (
	// signatureRe is a regular expression to capture the fields from the signature.
	signatureRe = regexp.MustCompile(
		`Signature keyId="(?P<keyId>[^"]+)",algorithm="(?P<algorithm>[^"]+)"` +
			`(,created=(?P<created>[0-9]+))?(,expires=(?P<expires>[0-9.]+))?,headers="(?P<headers>[^"]+)",signature="(?P<signature>[^"]+)"`)
)

// validateHttpAuthorizationSignature validates the HTTP signature in the HTTP request.
// The signature verification would normally be done by the server.
// Note: this is NOT a complete implementation of the HTTP signature validation. This code is for unit test purpose, do not use
// it for server side implementation.
// In particular, this code does not validate the calculation of the HTTP body digest.
func validateHttpAuthorizationSignature(t *testing.T, authConfig *sw.HttpSignatureAuth, r *http.Response) {
	authHeader := r.Request.Header.Get("Authorization")
	match := signatureRe.FindStringSubmatch(authHeader)
	if len(match) < 3 {
		t.Fatalf("Unexpected Authorization header: %s", authHeader)
	}
	result := make(map[string]string)
	for i, name := range signatureRe.SubexpNames() {
		if i != 0 && name != "" {
			result[name] = match[i]
		}
	}
	b64signature := result["signature"]
	fmt.Printf("Algorithm: '%s' Headers: '%s' b64signature: '%s'\n", result["algorithm"], result["headers"], b64signature)
	var sb bytes.Buffer
	fmt.Fprintf(&sb, "%s %s", strings.ToLower(r.Request.Method), r.Request.URL.EscapedPath())
	if r.Request.URL.RawQuery != "" {
		// The ":path" pseudo-header field includes the path and query parts
		// of the target URI (the "path-absolute" production and optionally a
		// '?' character followed by the "query" production (see Sections 3.3
		// and 3.4 of [RFC3986]
		fmt.Fprintf(&sb, "?%s", r.Request.URL.RawQuery)
	}
	requestTarget := sb.String()

	var signedHeaderKvs []string
	signedHeaders := strings.Split(result["headers"], " ")
	for _, h := range signedHeaders {
		var value string
		switch h {
		case sw.HttpSignatureParameterRequestTarget:
			value = requestTarget
		case sw.HttpSignatureParameterCreated:
			value = result["created"]
		case sw.HttpSignatureParameterExpires:
			value = result["expires"]
		default:
			value = r.Request.Header.Get(h)
		}
		signedHeaderKvs = append(signedHeaderKvs, fmt.Sprintf("%s: %s", h, value))
	}
	stringToSign := strings.Join(signedHeaderKvs, "\n")

	var h crypto.Hash
	switch result["algorithm"] {
	case sw.HttpSigningSchemeHs2019, sw.HttpSigningSchemeRsaSha512:
		h = crypto.SHA512
	case sw.HttpSigningSchemeRsaSha256:
		h = crypto.SHA256
	default:
		t.Fatalf("Unexpected signing algorithm: %s", result["algorithm"])
	}
	msgHash := h.New()
	if _, err := msgHash.Write([]byte(stringToSign)); err != nil {
		t.Fatalf("Unable to compute hash: %v", err)
	}
	d := msgHash.Sum(nil)
	var pub crypto.PublicKey
	var err error
	if pub, err = authConfig.GetPublicKey(); err != nil {
		t.Fatalf("Unable to get public key: %v", err)
	}
	if pub == nil {
		t.Fatalf("Public key is nil")
	}
	var signature []byte
	if signature, err = base64.StdEncoding.DecodeString(b64signature); err != nil {
		t.Fatalf("Failed to decode signature: %v", err)
	}
	switch publicKey := pub.(type) {
	case *rsa.PublicKey:
		// It could be PKCS1v15 or PSS signature
		var errPKCS1v15, errPSS error
		// In a server-side implementation, we wouldn't try to validate both signatures. The specific
		// signature algorithm would be derived from the key id. But here we just want to validate for unit test purpose.
		errPKCS1v15 = rsa.VerifyPKCS1v15(publicKey, h, d, signature)
		errPSS = rsa.VerifyPSS(publicKey, h, d, signature, nil)
		if errPKCS1v15 != nil && errPSS != nil {
			t.Fatalf("RSA Signature verification failed: %v. %v", errPKCS1v15, errPSS)
		}
	case *ecdsa.PublicKey:
		type ecdsaSignature struct {
			R, S *big.Int
		}
		var lEcdsa ecdsaSignature
		if _, err = asn1.Unmarshal(signature, &lEcdsa); err != nil {
			t.Fatalf("Unable to parse ECDSA signature: %v", err)
		}
		if !ecdsa.Verify(publicKey, d, lEcdsa.R, lEcdsa.S) {
			t.Fatalf("ECDSA Signature verification failed")
		}
	default:
		t.Fatalf("Unsupported public key: %T", pub)
	}
}

func TestHttpSignatureAuth(t *testing.T) {
	// Test with 'hs2019' signature scheme, and default signature algorithm (RSA SSA PKCS1.5)
	authConfig := sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeHs2019,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			"Host",                                 // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Date",                                 // The date and time at which the message was originated.
			"Content-Type",                         // The Media type of the body of the request.
			"Digest",                               // A cryptographic digest of the request body.
		},
	}
	executeHttpSignatureAuth(t, &authConfig, true)

	// Test with duplicate headers. This is invalid and should be rejected.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeHs2019,
		SignedHeaders: []string{"Host", "Date", "Host"},
	}
	executeHttpSignatureAuth(t, &authConfig, false)

	// Test with non-existent header. This is invalid and should be rejected.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeHs2019,
		SignedHeaders: []string{"Host", "Date", "Garbage-HeaderDoesNotExist"},
	}
	executeHttpSignatureAuth(t, &authConfig, false)

	// Test with 'Authorization' header in the signed headers. This is invalid and should be rejected.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeHs2019,
		SignedHeaders: []string{"Host", "Authorization"},
	}
	executeHttpSignatureAuth(t, &authConfig, false)

	// Specify signature max validity, but '(expires)' parameter is missing. This should cause an error.
	authConfig = sw.HttpSignatureAuth{
		KeyId:                "my-key-id",
		SigningScheme:        sw.HttpSigningSchemeHs2019,
		SignatureMaxValidity: 7 * time.Minute,
	}
	executeHttpSignatureAuth(t, &authConfig, false)

	// Specify invalid signature max validity. This should cause an error.
	authConfig = sw.HttpSignatureAuth{
		KeyId:                "my-key-id",
		SigningScheme:        sw.HttpSigningSchemeHs2019,
		SignatureMaxValidity: -3 * time.Minute,
	}
	executeHttpSignatureAuth(t, &authConfig, false)

	// Specify signature max validity and '(expires)' parameter.
	authConfig = sw.HttpSignatureAuth{
		KeyId:                "my-key-id",
		SigningScheme:        sw.HttpSigningSchemeHs2019,
		SignatureMaxValidity: time.Minute,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			sw.HttpSignatureParameterExpires,       // Time when signature expires.
		},
	}
	executeHttpSignatureAuth(t, &authConfig, true)

	// Specify '(expires)' parameter but no signature max validity.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeHs2019,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			sw.HttpSignatureParameterExpires,       // Time when signature expires.
		},
	}
	executeHttpSignatureAuth(t, &authConfig, false)

	// Test with empty signed headers. The client should automatically add the "(created)" parameter by default.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeHs2019,
		SignedHeaders: []string{},
	}
	executeHttpSignatureAuth(t, &authConfig, true)

	// Test with deprecated RSA-SHA512, some servers may still support it.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeRsaSha512,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			"Host",                                 // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Date",                                 // The date and time at which the message was originated.
			"Content-Type",                         // The Media type of the body of the request.
			"Digest",                               // A cryptographic digest of the request body.
		},
	}
	executeHttpSignatureAuth(t, &authConfig, true)

	// Test with deprecated RSA-SHA256, some servers may still support it.
	authConfig = sw.HttpSignatureAuth{
		KeyId:         "my-key-id",
		SigningScheme: sw.HttpSigningSchemeRsaSha256,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget, // The special (request-target) parameter expresses the HTTP request target.
			sw.HttpSignatureParameterCreated,       // Time when request was signed, formatted as a Unix timestamp integer value.
			"Host",                                 // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Date",                                 // The date and time at which the message was originated.
			"Content-Type",                         // The Media type of the body of the request.
			"Digest",                               // A cryptographic digest of the request body.
		},
	}
	executeHttpSignatureAuth(t, &authConfig, true)

	// Test with headers without date. This makes it possible to get a fixed signature, used for unit test purpose.
	// This should not be used in production code as it could potentially allow replay attacks.
	authConfig = sw.HttpSignatureAuth{
		KeyId:            "my-key-id",
		SigningScheme:    sw.HttpSigningSchemeHs2019,
		SigningAlgorithm: sw.HttpSigningAlgorithmRsaPKCS1v15,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget,
			"Host",         // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Content-Type", // The Media type of the body of the request.
			"Digest",       // A cryptographic digest of the request body.
		},
	}
	authorizationHeaderValue := executeHttpSignatureAuth(t, &authConfig, true)
	expectedSignature := "sXE2MDeW8os6ywv1oUWaFEPFcSPCEb/msQ/NZGKNA9Emm/e42axaAPojzfkZ9Hacyw/iS/5nH4YIkczMgXu3z5fAwFjumxtf3OxbqvUcQ80wvw2/7B5aQmsF6ZwrCFHZ+L/cj9/bg7L1EGUGtdyDzoRKti4zf9QF/03OsP7QljI="
	expectedAuthorizationHeader := fmt.Sprintf(
		`Signature keyId="my-key-id",`+
			`algorithm="hs2019",headers="(request-target) host content-type digest",`+
			`signature="%s"`, expectedSignature)
	if authorizationHeaderValue != expectedAuthorizationHeader {
		t.Errorf("Authorization header value is incorrect. Got\n'%s'\nbut expected\n'%s'", authorizationHeaderValue, expectedAuthorizationHeader)
	}

	// Test with PSS signature. The PSS signature creates a new signature every time it is invoked.
	authConfig = sw.HttpSignatureAuth{
		KeyId:            "my-key-id",
		SigningScheme:    sw.HttpSigningSchemeHs2019,
		SigningAlgorithm: sw.HttpSigningAlgorithmRsaPSS,
		SignedHeaders: []string{
			sw.HttpSignatureParameterRequestTarget,
			"Host",         // The Host request header specifies the domain name of the server, and optionally the TCP port number.
			"Content-Type", // The Media type of the body of the request.
			"Digest",       // A cryptographic digest of the request body.
		},
	}
	authorizationHeaderValue = executeHttpSignatureAuth(t, &authConfig, true)
	expectedSignature = `[a-zA-Z0-9+/]+=`
	expectedAuthorizationHeader = fmt.Sprintf(
		`Signature keyId="my-key-id",`+
			`algorithm="hs2019",headers="\(request-target\) host content-type digest",`+
			`signature="%s"`, expectedSignature)
	re := regexp.MustCompile(expectedAuthorizationHeader)
	if !re.MatchString(authorizationHeaderValue) {
		t.Errorf("Authorization header value is incorrect. Got\n'%s'\nbut expected\n'%s'", authorizationHeaderValue, expectedAuthorizationHeader)
	}
}

func TestInvalidHttpSignatureConfiguration(t *testing.T) {
	var err error
	var authConfig sw.HttpSignatureAuth

	authConfig = sw.HttpSignatureAuth{}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Key ID must be specified") {
		t.Fatalf("Invalid configuration: %v", err)
	}

	authConfig = sw.HttpSignatureAuth{
		KeyId: "my-key-id",
	}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Private key path must be specified") {
		t.Fatalf("Invalid configuration: %v", err)
	}

	authConfig = sw.HttpSignatureAuth{
		KeyId:          "my-key-id",
		PrivateKeyPath: "test.pem",
	}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Invalid signing scheme") {
		t.Fatalf("Invalid configuration: %v", err)
	}

	authConfig = sw.HttpSignatureAuth{
		KeyId:          "my-key-id",
		PrivateKeyPath: "test.pem",
		SigningScheme:  "garbage",
	}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Invalid signing scheme") {
		t.Fatalf("Invalid configuration: %v", err)
	}

	authConfig = sw.HttpSignatureAuth{
		KeyId:          "my-key-id",
		PrivateKeyPath: "test.pem",
		SigningScheme:  sw.HttpSigningSchemeHs2019,
		SignedHeaders:  []string{"foo", "bar", "foo"},
	}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "cannot have duplicate names") {
		t.Fatalf("Invalid configuration: %v", err)
	}

	authConfig = sw.HttpSignatureAuth{
		KeyId:          "my-key-id",
		PrivateKeyPath: "test.pem",
		SigningScheme:  sw.HttpSigningSchemeHs2019,
		SignedHeaders:  []string{"foo", "bar", "Authorization"},
	}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Signed headers cannot include the 'Authorization' header") {
		t.Fatalf("Invalid configuration: %v", err)
	}

	authConfig = sw.HttpSignatureAuth{
		KeyId:                "my-key-id",
		PrivateKeyPath:       "test.pem",
		SigningScheme:        sw.HttpSigningSchemeHs2019,
		SignedHeaders:        []string{"foo", "bar"},
		SignatureMaxValidity: -7 * time.Minute,
	}
	_, err = authConfig.ContextWithValue(context.Background())
	if err == nil || !strings.Contains(err.Error(), "Signature max validity must be a positive value") {
		t.Fatalf("Invalid configuration: %v", err)
	}
}
