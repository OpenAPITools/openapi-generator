package swagger

import (
)

type InlineResponse200 struct {
    Tags  []Tag  `json:"tags,omitempty"`
    Id  int64  `json:"id,omitempty"`
    Category  Object  `json:"category,omitempty"`
    Status  string  `json:"status,omitempty"`
    Name  string  `json:"name,omitempty"`
    PhotoUrls  []string  `json:"photoUrls,omitempty"`
    
}
