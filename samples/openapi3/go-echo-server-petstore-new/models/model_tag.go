package models

// Tag - A tag for a pet
type Tag struct {

	Id int64 `json:"id,omitempty"`

	Name string `json:"name,omitempty"`
}
