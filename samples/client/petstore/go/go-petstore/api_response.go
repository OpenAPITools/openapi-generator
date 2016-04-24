package swagger

import (
  "net/http"
)


type ApiResponse struct {
  *http.Response
  
  Message  string  `json:"message,omitempty"`
}

func NewApiResponse(r *http.Response) *ApiResponse {
  response := &ApiResponse{Response: r}

  return response
}

func NewApiResponseWithError(errorMessage string) *ApiResponse {
  response := &ApiResponse{Message: errorMessage}

  return response
}