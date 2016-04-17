package swagger

import (
    "strings"
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