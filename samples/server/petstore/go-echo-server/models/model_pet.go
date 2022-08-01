package models

// Pet - A pet for sale in the pet store
type Pet struct {

	Id int64 `json:"id,omitempty"`

	Category Category `json:"category,omitempty"`

	Name string `json:"name"`

	PhotoUrls []string `json:"photoUrls"`

	Tags []Tag `json:"tags,omitempty"`

	// pet status in the store
	// Deprecated
	Status string `json:"status,omitempty"`
}
