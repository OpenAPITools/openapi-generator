package swagger

import (
//    "encoding/json"
    "os"
//    "fmt"
    "log"
//    "net/url"
//    napping "github.com/jmcvetta/napping"
)

type ApiClient struct {
    basePath string
    logger *log.Logger
}

func NewApiClient() *ApiClient {
    return  &ApiClient {
        basePath:    "http://petstore.swagger.io/v2",
        logger:      log.New(os.Stderr, "", log.LstdFlags)}
}

func NewApiClientWithBasePath(basePath string) *ApiClient {
    return &ApiClient {
        basePath:    basePath,
        logger:      log.New(os.Stderr, "", log.LstdFlags)}
}


func (a *ApiClient) CallApi(basePath string, path string, httpMethod string, queryParams map[string]string, headerParams map[string]string, formParams map[string]string, fileParams map[string]string, bodyParams map[string]string, contentType []string)  {
    a.logger.Printf("Requesting %v\n%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v\n", basePath, path, httpMethod, queryParams, headerParams, formParams, fileParams, bodyParams, contentType)

}
