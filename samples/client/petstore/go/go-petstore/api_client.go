package swagger

import (
    "strings"
    "github.com/go-resty/resty"
    "errors"
    "reflect"
)

type ApiClient struct {

}

func (c *ApiClient) SelectHeaderContentType(contentTypes []string) string {
    if (len(contentTypes) == 0){
        return ""
    }
    if contains(contentTypes,"application/json") {
        return "application/json"
    }

    return contentTypes[0] // use the first content type specified in 'consumes'
}

func (c *ApiClient) SelectHeaderAccept(accepts []string) string {
    if (len(accepts) == 0){        
        return ""
    }

    if contains(accepts,"application/json"){        
        return "application/json"
    }

    return strings.Join(accepts,",")
}

func contains(source []string, containvalue string) bool {
    for _, a := range source {
        if strings.ToLower(a) == strings.ToLower(containvalue) {
            return true
        }
    }
    return false
}


func (c *ApiClient) CallApi(path string, method string,
    postBody interface{},
    headerParams map[string]string,
    queryParams map[string]string,
    formParams map[string]string,
    fileParams map[string]string) (*resty.Response, error) {

    request := prepareRequest(postBody, headerParams, queryParams, formParams, fileParams)

    switch strings.ToUpper(method) {
    case "GET":
        response, err := request.Get(path)
        return response, err
    case "POST":
        response, err := request.Post(path)
        return response, err
    case "PUT":
        response, err := request.Put(path)
        return response, err
    case "PATCH":
        response, err := request.Patch(path)
        return response, err
    case "DELETE":
        response, err := request.Delete(path)
        return response, err
    }

    return nil, errors.New("Invalid method " + method)
}

func (c *ApiClient) ParameterToString (obj interface{}) string {
    if reflect.TypeOf(obj).String() == "[]string" {
        return strings.Join(obj.([]string), ",")
    } else{
        return obj.(string)
    }
}

func prepareRequest(postBody interface{},
    headerParams map[string]string,
    queryParams map[string]string,
    formParams map[string]string,
    fileParams map[string]string) *resty.Request {

    request := resty.R()

    request.SetBody(postBody)

    // add header parameter, if any
    if len(headerParams) > 0 {
        request.SetHeaders(headerParams)
    }
    
    // add query parameter, if any
    if len(queryParams) > 0 {
        request.SetQueryParams(queryParams)
    }

    // add form parameter, if any
    if len(fileParams) > 0 {
        request.SetFormData(formParams)
    }

    // add file parameter, if any
    if len(fileParams) > 0 {
        request.SetFiles(fileParams)
    }
    
    return request
}