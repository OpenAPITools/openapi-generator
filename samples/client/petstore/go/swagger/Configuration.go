package swagger

import (

)

type Configuration struct {
    UserName  string  `json:"userName,omitempty"`
    ApiKey  string  `json:"apiKey,omitempty"`
    Debug  bool  `json:"debug,omitempty"`
    DebugFile  string  `json:"debugFile,omitempty"`
    OAuthToken  string  `json:"oAuthToken,omitempty"`
    Timeout  int  `json:"timeout,omitempty"`
    BasePath  string  `json:"basePath,omitempty"`
    Host  string  `json:"host,omitempty"`
    Scheme  string  `json:"scheme,omitempty"`
    DefaultHeader map[string]string `json:"defaultHeader,omitempty"`
}

func NewConfiguration() *Configuration {
    defaultHeader := make(map[string]string)
    return &Configuration{
        BasePath: "http://petstore.swagger.io/v2",
        UserName: "",
        Debug: false,
        DefaultHeader: defaultHeader,
        }
}

func (c *Configuration) AddDefaultHeader(key string, value string) {
    c.DefaultHeader[key] = value
}