package petstore

import (
	"encoding/base64"
)

type Configuration struct {
	UserName      string            `json:"userName,omitempty"`
	Password      string            `json:"password,omitempty"`
	APIKeyPrefix  map[string]string `json:"APIKeyPrefix,omitempty"`
	APIKey        map[string]string `json:"APIKey,omitempty"`
	debug         bool              `json:"debug,omitempty"`
	DebugFile     string            `json:"debugFile,omitempty"`
	OAuthToken    string            `json:"oAuthToken,omitempty"`
	Timeout       int               `json:"timeout,omitempty"`
	BasePath      string            `json:"basePath,omitempty"`
	Host          string            `json:"host,omitempty"`
	Scheme        string            `json:"scheme,omitempty"`
	AccessToken   string            `json:"accessToken,omitempty"`
	DefaultHeader map[string]string `json:"defaultHeader,omitempty"`
	UserAgent     string            `json:"userAgent,omitempty"`
	APIClient     APIClient         `json:"APIClient,omitempty"`
}

func NewConfiguration() *Configuration {
	return &Configuration{
		BasePath:      "http://petstore.swagger.io/v2",
		UserName:      "",
		debug:         false,
		DefaultHeader: make(map[string]string),
		APIKey:        make(map[string]string),
		APIKeyPrefix:  make(map[string]string),
		UserAgent:     "Swagger-Codegen/1.0.0/go",
	}
}

func (c *Configuration) GetBasicAuthEncodedString() string {
	return base64.StdEncoding.EncodeToString([]byte(c.UserName + ":" + c.Password))
}

func (c *Configuration) AddDefaultHeader(key string, value string) {
	c.DefaultHeader[key] = value
}

func (c *Configuration) GetAPIKeyWithPrefix(APIKeyIdentifier string) string {
	if c.APIKeyPrefix[APIKeyIdentifier] != "" {
		return c.APIKeyPrefix[APIKeyIdentifier] + " " + c.APIKey[APIKeyIdentifier]
	}

	return c.APIKey[APIKeyIdentifier]
}

func (c *Configuration) SetDebug(enable bool) {
	c.debug = enable
}

func (c *Configuration) GetDebug() bool {
	return c.debug
}
