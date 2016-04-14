package swagger

import (
    "encoding/base64"
)

type Configuration struct {
    UserName  string  `json:"userName,omitempty"`
    Password  string  `json:"password,omitempty"`
    ApiKeyPrefix map[string] string `json:"apiKeyPrefix,omitempty"`
    ApiKey map[string] string  `json:"apiKey,omitempty"`
    Debug  bool  `json:"debug,omitempty"`
    DebugFile  string  `json:"debugFile,omitempty"`
    OAuthToken  string  `json:"oAuthToken,omitempty"`
    Timeout  int  `json:"timeout,omitempty"`
    BasePath  string  `json:"basePath,omitempty"`
    Host  string  `json:"host,omitempty"`
    Scheme  string  `json:"scheme,omitempty"`
    AccessToken string `json:"accessToken,omitempty"`
    DefaultHeader map[string]string `json:"defaultHeader,omitempty"`
    UserAgent string `json:"userAgent,omitempty"`
    ApiClient ApiClient `json:"apiClient,omitempty"`
}

func NewConfiguration() *Configuration {
    return &Configuration{
        BasePath: "http://petstore.swagger.io/v2",
        UserName: "",
        Debug: false,
        DefaultHeader: make(map[string]string),
        ApiKey: make(map[string]string),
        ApiKeyPrefix: make(map[string]string),
        UserAgent: "Swagger-Codegen/1.0.0/go",
        }
}

func (c *Configuration) GetBasicAuthEncodedString() string {
    return base64.StdEncoding.EncodeToString([]byte(c.UserName  + ":" + c.Password))
}

func (c *Configuration) AddDefaultHeader(key string, value string) {
    c.DefaultHeader[key] = value
}

func (c *Configuration) GetApiKeyWithPrefix(apiKeyIdentifier string) string {
    if c.ApiKeyPrefix[apiKeyIdentifier] != ""{
        return c.ApiKeyPrefix[apiKeyIdentifier] + " " + c.ApiKey[apiKeyIdentifier]
    }
        
    return c.ApiKey[apiKeyIdentifier]
}