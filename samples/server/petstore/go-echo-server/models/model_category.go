package models

// Category - A category for a pet
type Category struct {

	Id int64 `json:"id,omitempty"`

	Name string `json:"name,omitempty"`
}
