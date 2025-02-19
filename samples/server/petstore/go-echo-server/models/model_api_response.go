package models

// ApiResponse - Describes the result of uploading an image resource
type ApiResponse struct {

	Code int32 `json:"code,omitempty"`

	Type string `json:"type,omitempty"`

	Message string `json:"message,omitempty"`
}
